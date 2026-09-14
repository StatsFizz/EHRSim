################################################################################
#
#   Script:       P0_f_Init_functions_treatment_effects.R
#
#   Purpose:      Create helper function to extract true treatment effects of 
#                 counter-factual treatment strategies sourced in 7b
#                 
#
################################################################################

# Natural and never are identical across stopping versions. Keep both versions
# during calculation, then return one copy of each reference world.
clean_treatment_run_output <- function(run, specs) {
  primary_truth_version <- run$estimand$versions[1]
  reference_strategies <- c(
    "natural",
    paste0("forced_no_", specs$drug_name)
  )

  clean_table <- function(table) {
    if (!is.data.frame(table) ||
        !all(c("truth_version", "strategy") %in% names(table))) {
      return(table)
    }

    table <- copy(table)
    table[
      truth_version == primary_truth_version |
        !strategy %in% reference_strategies
    ]
  }

  run$truth_worlds <- clean_table(run$truth_worlds)
  run$truth_treatment_effects <- lapply(
    run$truth_treatment_effects,
    clean_table
  )
  run
}

# Function to recalculate counter-factual worlds cause-specific hazards,
# cumulative hazards and survival

recalculate_strategy_hazards <- function(dt) {
  setorder(dt, truth_version, patient_id, strategy, follow_start, follow_end)
  
  dt[, `:=`(
    h_1 = exp(lnhr_all_1),
    h_2 = exp(lnhr_all_2),
    h_3 = exp(lnhr_all_3)
  )]
  
  dt[, `:=`(
    h_all_cause = h_1 + h_2 + h_3,
    accum_H_1 = h_1 * interval_length,
    accum_H_2 = h_2 * interval_length,
    accum_H_3 = h_3 * interval_length
  )]
  
  dt[, accum_all_cause := accum_H_1 + accum_H_2 + accum_H_3]
  
  dt[, `:=`(
    H_1 = cumsum(accum_H_1),
    H_2 = cumsum(accum_H_2),
    H_3 = cumsum(accum_H_3),
    H_all_cause = cumsum(accum_all_cause)
  ), by = .(truth_version, patient_id, strategy)]
  
  dt[, `:=`(
    S_1 = exp(-H_1),
    S_2 = exp(-H_2),
    S_3 = exp(-H_3),
    S_all_cause = exp(-H_all_cause)
  )]
  
  dt[]
}

#########################################################
############ Function to compute patient-level survival/risk/RMST at one horizon

calc_horizon_summary <- function(dt, horizon) {
  x <- copy(dt)
  
  max_follow <- x[
    ,
    .(max_follow_end = max(follow_end, na.rm = TRUE)),
    by = .(truth_version, patient_id, strategy)
  ]
  
  # Complete-horizon estimates are averaged only among patients observable to
  # the requested horizon. BE WARY wehn interpreting 5/10-year
  # summaries, there is are changes to the target population.
  max_follow[, complete_horizon := max_follow_end >= horizon]
  
  x <- x[follow_start < horizon]
  x[, `:=`(
    trimmed_end = pmin(follow_end, horizon),
    trimmed_length = pmax(0, pmin(follow_end, horizon) - follow_start)
  )]
  
  # Survival at the start of each interval is needed for exact integration
  # under piecewise-constant hazards.
  x[, H_start_all := H_all_cause - accum_all_cause]
  x[, S_start_all := exp(-H_start_all)]
  
  x[, rmst_piece := fifelse(
    h_all_cause > 0,
    S_start_all * (1 - exp(-h_all_cause * trimmed_length)) / h_all_cause,
    S_start_all * trimmed_length
  )]
  
  horizon_out <- x[
    ,
    .(
      H_horizon_available = sum(h_all_cause * trimmed_length, na.rm = TRUE),
      rmst_available = sum(rmst_piece, na.rm = TRUE)
    ),
    by = .(truth_version, patient_id, strategy)
  ]
  
  horizon_out <- merge(
    max_follow,
    horizon_out,
    by = c("truth_version", "patient_id", "strategy"),
    all.x = TRUE
  )
  
  horizon_out[is.na(H_horizon_available), H_horizon_available := 0]
  horizon_out[is.na(rmst_available), rmst_available := 0]
  
  horizon_out[, `:=`(
    survival_available = exp(-H_horizon_available),
    risk_available = 1 - exp(-H_horizon_available)
  )]
  
  horizon_out[, `:=`(
    survival = fifelse(complete_horizon, survival_available, NA_real_),
    risk = fifelse(complete_horizon, risk_available, NA_real_),
    rmst = fifelse(complete_horizon, rmst_available, NA_real_),
    horizon_years = horizon
  )]
  
  horizon_out[]
}

########################################################
###### Marking natural treatment activity on intervals.

mark_natural_treatment_exposure <- function(treatment_cohort, medication, eligible,
                                            drug_group){
  treatment_med <- medication[
    patient_id %in% eligible$patient_id & drug %chin% drug_group,
    .(patient_id, date, days)
  ]
  
  setorder(treatment_med, patient_id, date)
  treatment_med[, days_clean := fifelse(is.na(days), 0, days)]
  treatment_med[, date_end := date + days_clean]
  treatment_med[, grace_end := date_end + 0.25 * days_clean]
  treatment_med[
    ,
    prev_grace_end := shift(cummax(as.numeric(grace_end))),
    by = patient_id
  ]
  treatment_med[
    ,
    new_episode := is.na(prev_grace_end) |
      as.numeric(date) > prev_grace_end
  ]
  treatment_med[, episode := cumsum(new_episode), by = patient_id]
  
  treatment_eps <- treatment_med[
    ,
    .(
      treatment_start_date = first(date),
      treatment_stop_date = max(grace_end, na.rm = TRUE)
    ),
    by = .(patient_id, episode)
  ]
  
  treatment_eps <- merge(
    treatment_eps,
    eligible[, .(patient_id, time_zero)],
    by = "patient_id"
  )
  
  treatment_eps[, `:=`(
    treatment_start_follow = as.numeric(treatment_start_date - time_zero) / 365.25,
    treatment_end_follow = as.numeric(treatment_stop_date - time_zero) / 365.25
  )]
  
  treatment_eps[is.na(treatment_end_follow), treatment_end_follow := Inf]
  treatment_eps <- treatment_eps[treatment_end_follow > 0]
  
  treatment_cohort[, treatment_active_natural := 0L]
  
  if (nrow(treatment_eps) > 0) {
      treatment_overlap_check <- merge(
      treatment_cohort[, .(interval_id, patient_id, follow_start, follow_end)],
      treatment_eps[, .(
        patient_id,
        episode,
        treatment_start_follow,
        treatment_end_follow
      )],
      by = "patient_id",
      allow.cartesian = TRUE
    )
    
    active_interval_ids <- unique(
      treatment_overlap_check[
        follow_start < treatment_end_follow &
          follow_end > treatment_start_follow,
        interval_id
      ]
    )
    
    treatment_cohort[
      interval_id %in% active_interval_ids,
      treatment_active_natural := 1L
    ]
  }
  
  treatment_cohort[]
}

#####################################################################
###### Adding first observed post-time-zero stop event/date.

add_stop_triggers<- function(eligible, comorbid_dt, stop_trigger_cols = NULL){
  stopper_cols <- intersect(stop_trigger_cols,
    names(comorbid_dt)
  )
  
  eligible[, `:=`(
    first_stop_date = as.Date(NA),
    first_stop_event = NA_character_
  )]
  
  if (length(stopper_cols) > 0) {
    stopper_dt <- unique(comorbid_dt[, c("patient_id", stopper_cols), with = FALSE])
    stopper_long <- melt(
      stopper_dt,
      id.vars = "patient_id",
      measure.vars = stopper_cols,
      variable.name = "first_stop_event",
      value.name = "first_stop_date",
      na.rm = TRUE
    )
    
    stopper_long <- merge(
      stopper_long,
      eligible[, .(patient_id, time_zero, enddate_init)],
      by = "patient_id"
    )
    
    stopper_long <- stopper_long[
      first_stop_date > time_zero & first_stop_date <= enddate_init
    ]
    
    if (nrow(stopper_long) > 0) {
      stopper_long[, first_stop_event := sub("_event_date$", "", first_stop_event)]
      setorder(stopper_long, patient_id, first_stop_date, first_stop_event)
      first_stopper <- stopper_long[, .SD[1], by = patient_id]
      
      eligible[
        first_stopper,
        `:=`(
          first_stop_date = i.first_stop_date,
          first_stop_event = i.first_stop_event
        ),
        on = "patient_id"
      ]
    }
  }
  eligible[]
}


################################################################################
# Trims eligible hazard intervals to post-time-zero follow-up and splits 
# at stop dates.

prepare_post_time_zero_intervals<-function(all_hazard_mod, eligible){
  treatment_cohort <- copy(all_hazard_mod[patient_id %in% eligible$patient_id])
  treatment_cohort <- merge(treatment_cohort, eligible, by = "patient_id")
  
  treatment_cohort <- treatment_cohort[!is.na(date_end) & date_end > time_zero]
  treatment_cohort[date < time_zero, date := time_zero]
  
  treatment_cohort[, split_at_stop_date := !is.na(first_stop_date) &
              date < first_stop_date &
              first_stop_date < date_end]
  
  rows_to_split <- treatment_cohort[split_at_stop_date == TRUE]
  rows_not_split <- treatment_cohort[split_at_stop_date == FALSE]
  
  if (nrow(rows_to_split) > 0) {
    before_stop <- copy(rows_to_split)
    before_stop[, date_end := first_stop_date]
    
    after_stop <- copy(rows_to_split)
    after_stop[, date := first_stop_date]
    
    treatment_cohort <- rbindlist(
      list(rows_not_split, before_stop, after_stop),
      use.names = TRUE,
      fill = TRUE
    )
  } else {
    treatment_cohort <- rows_not_split
  }
  
  treatment_cohort[, split_at_stop_date := NULL]
  setorder(treatment_cohort, patient_id, date, date_end)
  
  treatment_cohort[, `:=`(
    interval_length = as.numeric(date_end - date) / 365.25,
    follow_start = as.numeric(date - time_zero) / 365.25,
    follow_end = as.numeric(date_end - time_zero) / 365.25
  )]
  
  treatment_cohort <- treatment_cohort[interval_length > 0]
  setorder(treatment_cohort, patient_id, follow_start, follow_end)
  treatment_cohort[, interval_id := .I]
  
  # Forced treatment is active after time zero until the first stopping date.
  # If there is no post-time-zero stop date, treatment stays active.
  treatment_cohort[, treatment_active_with_stopping := as.integer(
    is.na(first_stop_date) | date < first_stop_date
  )]
  treatment_cohort[]
}

################################################################################
# Build the different strategy worlds by applying strategy-specific treatment 
# exposure and recalculates hazards.
# Assumption: All that gets changed is the affected cause-specific log hazard
# linear combination.
build_strategy_world <- function(base_dt,
                                 truth_version,
                                 strategy_name,
                                 treatment_active,
                                 treatment_start_log_hr,
                                 affected_hazard_column = "lnhr_all_1",
                                 baseline_hazard_column = "lnhr_affected_no_treatment") {
  out <- copy(base_dt)
  out[, `:=`(
    truth_version = truth_version,
    strategy = strategy_name
  )]
  
  treatment_active_values <- as.integer(treatment_active)
  out[, treatment_active := treatment_active_values]
  
  out[, treatment_log_hr_strategy := fifelse(
    treatment_active == 1L,
    treatment_start_log_hr,
    0
  )]
  
  out[
    ,
    (affected_hazard_column) :=
      get(baseline_hazard_column) + treatment_log_hr_strategy
  ]
  
  recalculate_strategy_hazards(out)
}

################################################################################
# Build a cohort based on desired specs, so create cohort to combat prevalent
# users, ensure initiation and not continuation, and then add labels.
# Builds the trigger-based treatment-new-user target population.

build_new_user_trigger_cohort <- function(comorbid_dt, patient, medication,
                                          drug_group, population,
                                          start_trigger_cols, time_zero_col){
  
  trigger_cols <- intersect(start_trigger_cols, names(comorbid_dt))
  
  if (length(trigger_cols) == 0) {
    stop(
      'Requested start triggers not found in comorbid_dt',
      call. = FALSE
    )
  }
  
  trigger_wide <- unique(comorbid_dt[,c("patient_id", trigger_cols), with = FALSE])
  
  trigger_wide <- melt(trigger_wide,
                       id.vars = "patient_id",
                       measure.vars = trigger_cols,
                       variable.name = "trigger_event",
                       value.name = "trigger_date",
                       na.rm = T)

  if (nrow(trigger_wide) == 0L) {
    stop(
      "No eligible patients found for treatment population '",
      population,
      "': the requested start triggers contain no observed dates.",
      call. = FALSE
    )
  }
  
  first_trigger <- trigger_wide
  
  setorder(first_trigger, patient_id, trigger_date, trigger_event)
  
  first_trigger <- first_trigger[
    ,
    .SD[1], # 1 row per patient
    by = patient_id
  ]
  
  setnames(first_trigger, "trigger_date", time_zero_col)
  
  first_treatment <- medication[
    drug %chin% drug_group,
    .(first_treatment_date = min(date, na.rm = TRUE)),
    by = patient_id
  ]
  
  eligible <- merge(first_trigger, first_treatment, by = "patient_id", all.x = TRUE)
  eligible <- merge(
    patient[, .(patient_id, dob, sex, reg_date, enddate_init)],
    eligible,
    by = "patient_id"
  )
  
  time_zero_values <- eligible[[time_zero_col]]
  eligible <- eligible[
    is.na(first_treatment_date) | first_treatment_date >= time_zero_values
  ]
  
  time_zero_values <- eligible[[time_zero_col]]
  eligible <- eligible[
    reg_date <= time_zero_values & enddate_init >= time_zero_values
  ]

  if (nrow(eligible) == 0L) {
    stop(
      "No eligible patients found for treatment population '",
      population,
      "' after applying new-user and follow-up eligibility criteria.",
      call. = FALSE
    )
  }
  
  eligible[, `:=`(
    population = population,
    time_zero = get(time_zero_col)
  )]
  eligible[]
}

################################################################################
# Restricts latent-eligible patients to observable EHR follow-up at time zero.

restrict_eligible_to_ehr_observation <- function(
    eligible,
    ehr_observation_window = NULL) {
  if (is.null(ehr_observation_window)) {
    return(eligible[])
  }

  if (!is.data.frame(ehr_observation_window)) {
    stop("ehr_observation_window must be tabular.", call. = FALSE)
  }

  required_columns <- c(
    "patient_id", "ehr_reg_date", "transfer_date",
    "death_date_official", "study_end"
  )
  missing_columns <- setdiff(required_columns, names(ehr_observation_window))
  if (length(missing_columns) > 0L) {
    stop(
      "ehr_observation_window is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(ehr_observation_window$patient_id)) {
    stop("ehr_observation_window must contain one row per patient.",
         call. = FALSE)
  }

  eligible_columns <- names(eligible)
  observation <- as.data.table(copy(ehr_observation_window))
  observed_eligible <- merge(
    copy(eligible),
    observation[, ..required_columns],
    by = "patient_id",
    all.x = TRUE
  )
  observed_eligible <- observed_eligible[
    !is.na(ehr_reg_date) &
      ehr_reg_date <= time_zero &
      (is.na(transfer_date) | time_zero <= transfer_date) &
      (is.na(death_date_official) | time_zero <= death_date_official) &
      !is.na(study_end) & time_zero <= study_end,
    ..eligible_columns
  ]

  if (nrow(observed_eligible) == 0L) {
    stop(
      "No eligible patients remain after requiring P13 EHR observability at time zero.",
      call. = FALSE
    )
  }

  observed_eligible[]
}

################################################################################
# Builds plot-ready patient-level and mean survival curves on a regular grid.

build_survival_curve_tables <- function(strategy_worlds,
                                        max_horizon,
                                        grid_step = 1 / 12) {
  grid_dt <- data.table(grid_time = seq(0, max_horizon, by = grid_step))
  patient_strategy <- unique(
    strategy_worlds[, .(truth_version, patient_id, strategy)]
  )
  curve_grid <- patient_strategy[
    ,
    .(grid_time = grid_dt$grid_time),
    by = .(truth_version, patient_id, strategy)
  ]
  
  curve_grid[, `:=`(
    grid_start = grid_time,
    grid_end = grid_time
  )]
  
  curve_intervals <- strategy_worlds[
    ,
    .(
      truth_version,
      patient_id,
      strategy,
      follow_start,
      follow_end,
      H_all_cause,
      accum_all_cause,
      h_all_cause
    )
  ]
  
  setkey(
    curve_intervals,
    truth_version,
    patient_id,
    strategy,
    follow_start,
    follow_end
  )
  setkey(
    curve_grid,
    truth_version,
    patient_id,
    strategy,
    grid_start,
    grid_end
  )
  
  survival_curve_patient <- foverlaps(
    curve_grid,
    curve_intervals,
    by.x = c(
      "truth_version",
      "patient_id",
      "strategy",
      "grid_start",
      "grid_end"
    ),
    by.y = c(
      "truth_version",
      "patient_id",
      "strategy",
      "follow_start",
      "follow_end"
    ),
    type = "within",
    nomatch = NA
  )
  
  # If a grid point falls exactly on an interval boundary, keep the later interval.
  setorder(
    survival_curve_patient,
    truth_version,
    patient_id,
    strategy,
    grid_time,
    -follow_start
  )
  
  survival_curve_patient <- survival_curve_patient[
    ,
    .SD[1],
    by = .(truth_version, patient_id, strategy, grid_time)
  ]
  
  survival_curve_patient[
    !is.na(follow_start),
    `:=`(
      H_start = H_all_cause - accum_all_cause,
      time_into_interval = grid_time - follow_start
    )
  ]
  
  survival_curve_patient[
    !is.na(follow_start),
    `:=`(
      H_grid = H_start + h_all_cause * time_into_interval,
      S_grid = exp(-(H_start + h_all_cause * time_into_interval))
    )
  ]
  
  survival_curve_patient <- survival_curve_patient[
    ,
    .(truth_version, patient_id, strategy, grid_time, H_grid, S_grid)
  ]
  
  survival_curve_mean <- survival_curve_patient[
    ,
    .(
      mean_S = mean(S_grid, na.rm = TRUE),
      n_contributing = sum(!is.na(S_grid))
    ),
    by = .(truth_version, grid_time, strategy)
  ]
  
  list(
    patient = survival_curve_patient,
    mean = survival_curve_mean
  )
}

################################################################################
# Builds patient- and strategy-level horizon summaries for survival, risk, and
# RMST across all requested horizons.

build_treatment_horizon_tables <- function(strategy_worlds, horizons) {
  horizon_patient <- rbindlist(
    lapply(horizons, function(horizon) {
      calc_horizon_summary(strategy_worlds, horizon)
    }),
    use.names = TRUE
  )
  
  horizon_strategy <- horizon_patient[
    ,
    .(
      n_patients = uniqueN(patient_id),
      n_complete = sum(complete_horizon),
      mean_survival = mean(survival, na.rm = TRUE),
      mean_risk = mean(risk, na.rm = TRUE),
      mean_rmst = mean(rmst, na.rm = TRUE)
    ),
    by = .(truth_version, horizon_years, strategy)
  ]
  
  list(
    patient = horizon_patient,
    strategy = horizon_strategy
  )
}

################################################################################
# Builds patient- and population-level contrasts between a forced treatment
# strategy and a forced no-treatment comparator.

build_strategy_contrast_tables <- function(horizon_patient,
                                           treatment_strategy,
                                           comparator_strategy,
                                           natural_strategy = "natural") {
  horizon_wide <- dcast(
    horizon_patient,
    truth_version + patient_id + horizon_years + complete_horizon ~ strategy,
    value.var = c("survival", "risk", "rmst")
  )
  
  survival_treatment_col <- paste0("survival_", treatment_strategy)
  survival_comparator_col <- paste0("survival_", comparator_strategy)
  survival_natural_col <- paste0("survival_", natural_strategy)
  risk_treatment_col <- paste0("risk_", treatment_strategy)
  risk_comparator_col <- paste0("risk_", comparator_strategy)
  risk_natural_col <- paste0("risk_", natural_strategy)
  rmst_treatment_col <- paste0("rmst_", treatment_strategy)
  rmst_comparator_col <- paste0("rmst_", comparator_strategy)
  rmst_natural_col <- paste0("rmst_", natural_strategy)
  
  contrast_cols <- c(
    survival_treatment_col,
    survival_comparator_col,
    survival_natural_col,
    risk_treatment_col,
    risk_comparator_col,
    risk_natural_col,
    rmst_treatment_col,
    rmst_comparator_col,
    rmst_natural_col
  )
  missing_cols <- setdiff(contrast_cols, names(horizon_wide))
  if (length(missing_cols) > 0) {
    stop(
      "Cannot build treatment contrasts. Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  id_cols <- c("truth_version", "patient_id", "horizon_years", "complete_horizon")
  contrast_patient <- horizon_wide[, c(id_cols, contrast_cols), with = FALSE]
  contrast_patient[, `:=`(
    survival_diff = get(survival_treatment_col) - get(survival_comparator_col),
    risk_diff = get(risk_treatment_col) - get(risk_comparator_col),
    rmst_diff = get(rmst_treatment_col) - get(rmst_comparator_col),
    rmst_days_diff = 365.25 *
      (get(rmst_treatment_col) - get(rmst_comparator_col))
  )]
  setcolorder(
    contrast_patient,
    c(
      id_cols,
      survival_treatment_col,
      survival_comparator_col,
      survival_natural_col,
      "survival_diff",
      risk_treatment_col,
      risk_comparator_col,
      risk_natural_col,
      "risk_diff",
      rmst_treatment_col,
      rmst_comparator_col,
      rmst_natural_col,
      "rmst_diff",
      "rmst_days_diff"
    )
  )
  
  contrast_population <- contrast_patient[
    ,
    .(
      n_patients = uniqueN(patient_id),
      n_complete = sum(complete_horizon),
      mean_survival_diff = mean(survival_diff, na.rm = TRUE),
      mean_risk_diff = mean(risk_diff, na.rm = TRUE),
      mean_rmst_diff = mean(rmst_diff, na.rm = TRUE),
      mean_rmst_days_diff = mean(rmst_days_diff, na.rm = TRUE)
    ),
    by = .(truth_version, horizon_years)
  ]
  
  list(
    wide = horizon_wide,
    patient = contrast_patient,
    population = contrast_population
  )
}

################################################################################
# Builds canonical persistent strategy-world summaries: natural, never, no_stop,
# and stop. This is the cleaner four-row-per-horizon view layered on top of the
# legacy no-stopping/with-stopping strategy layout.

build_persistent_world_tables <- function(horizon_strategy, estimand, specs) {
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  has_stop_world <- length(estimand$versions) >= 2L
  
  horizon_world <- copy(horizon_strategy)
  horizon_world[, strategy_world := fcase(
    truth_version == estimand$versions[1] & strategy == "natural", "natural",
    truth_version == estimand$versions[1] &
      strategy == forced_no_drug_strategy, "never",
    truth_version == estimand$versions[1] &
      strategy == forced_drug_strategy, "no_stop",
    has_stop_world &
      truth_version == estimand$versions[2] &
      strategy == forced_drug_strategy, "stop",
    default = NA_character_
  )]
  
  horizon_world <- horizon_world[!is.na(strategy_world)]
  setcolorder(
    horizon_world,
    c(
      "truth_version",
      "horizon_years",
      "strategy_world",
      "strategy",
      "n_patients",
      "n_complete",
      "mean_survival",
      "mean_risk",
      "mean_rmst"
    )
  )
  
  horizon_world_wide <- dcast(
    horizon_world,
    horizon_years ~ strategy_world,
    value.var = c(
      "n_patients",
      "n_complete",
      "mean_survival",
      "mean_risk",
      "mean_rmst"
    )
  )
  
  contrast_world_population <- horizon_world_wide[
    ,
    .(
      horizon_years,
      n_patients = n_patients_never,
      n_complete = n_complete_never,
      mean_survival_diff_natural_vs_never =
        mean_survival_natural - mean_survival_never,
      mean_survival_diff_no_stop_vs_never =
        mean_survival_no_stop - mean_survival_never,
      mean_risk_diff_natural_vs_never =
        mean_risk_natural - mean_risk_never,
      mean_risk_diff_no_stop_vs_never =
        mean_risk_no_stop - mean_risk_never,
      mean_rmst_diff_natural_vs_never =
        mean_rmst_natural - mean_rmst_never,
      mean_rmst_diff_no_stop_vs_never =
        mean_rmst_no_stop - mean_rmst_never,
      mean_rmst_days_diff_natural_vs_never =
        365.25 * (mean_rmst_natural - mean_rmst_never),
      mean_rmst_days_diff_no_stop_vs_never =
        365.25 * (mean_rmst_no_stop - mean_rmst_never)
    )
  ]
  
  if ("mean_survival_stop" %in% names(horizon_world_wide)) {
    contrast_world_population[
      horizon_world_wide,
      `:=`(
        mean_survival_diff_stop_vs_never =
          i.mean_survival_stop - i.mean_survival_never,
        mean_risk_diff_stop_vs_never =
          i.mean_risk_stop - i.mean_risk_never,
        mean_rmst_diff_stop_vs_never =
          i.mean_rmst_stop - i.mean_rmst_never,
        mean_rmst_days_diff_stop_vs_never =
          365.25 * (i.mean_rmst_stop - i.mean_rmst_never)
      ),
      on = "horizon_years"
    ]
  }
  
  list(
    horizon_world = horizon_world,
    horizon_world_wide = horizon_world_wide,
    contrast_world_population = contrast_world_population
  )
}

################################################################################
# Builds small descriptive tables for treatment coefficients and estimand
# metadata.

build_treatment_coefficient_table <- function(estimand, specs,
                                              treatment_start_log_hr,
                                              treatment_stop_log_hr) {
  data.table(
    truth_version = estimand$versions,
    drug = specs$drug_name,
    strategy_trigger = specs$start_trigger_label,
    affected_cause = specs$affected_cause,
    log_hr_while_active = treatment_start_log_hr,
    hr_while_active = exp(treatment_start_log_hr),
    comparator = paste0("no_", specs$drug_name),
    log_hr_inactive = 0,
    hr_inactive = 1,
    log_hr_stop = treatment_stop_log_hr,
    hr_stop = exp(treatment_stop_log_hr)
  )
}

build_treatment_metadata_table <- function(estimand) {
  rbindlist(
    list(
      data.table(
        field = c(
          "population",
          "time_zero",
          "treatment",
          "comparator",
          "natural_strategy",
          "outcome",
          "secondary_outcome",
          "primary_estimand"
        ),
        value = c(
          estimand$population,
          estimand$time_zero,
          estimand$treatment,
          estimand$comparator,
          estimand$natural_strategy,
          estimand$outcome,
          estimand$secondary_outcome,
          estimand$primary_estimand
        )
      ),
      data.table(field = "horizon", value = as.character(estimand$horizon)),
      data.table(field = "version", value = estimand$versions)
    ),
    use.names = TRUE
  )
}

################################################################################
# Builds interval-level survival export with generic treatment columns.

build_survival_interval_table <- function(strategy_worlds) {
  interval_cols <- c(
    "truth_version",
    "patient_id",
    "strategy",
    "follow_start",
    "follow_end",
    "interval_length",
    "first_stop_date",
    "first_stop_event",
    "treatment_active",
    "treatment_active_natural",
    "treatment_log_hr_natural",
    "treatment_log_hr_strategy",
    "lnhr_affected_no_treatment",
    "lnhr_all_1",
    "lnhr_all_2",
    "lnhr_all_3",
    "h_1",
    "h_2",
    "h_3",
    "h_all_cause",
    "H_all_cause",
    "S_all_cause"
  )
  optional_treatment_cols <- intersect(
    c(
      "treatment_active_with_stopping",
      "treatment_active_fixed_course_no_stopping",
      "treatment_active_fixed_course_with_stopping",
      "treatment_active_episodic_no_stopping",
      "treatment_active_episodic_with_stopping"
    ),
    names(strategy_worlds)
  )
  interval_cols <- c(interval_cols, optional_treatment_cols)
  
  missing_cols <- setdiff(interval_cols, names(strategy_worlds))
  if (length(missing_cols) > 0) {
    stop(
      "Cannot build survival interval table. Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  strategy_worlds[, interval_cols, with = FALSE]
}


################################################################################
################################################################################

run_persistent_treatment_strategy <- function(patient, comorbid_dt, medication,
                                              drug_groups, all_hazard_mod,
                                              specs, estimand,
                                              treatment_start_log_hr,
                                              ehr_observation_window = NULL) {
  treatment_drugs <- drug_groups[[specs$drug_name]]
  if (is.null(treatment_drugs)) {
    treatment_drugs <- specs$drug_name
  }
  
  has_stop_triggers <- has_treatment_stop_triggers(specs)
  stop_trigger_cols <- if (has_stop_triggers) specs$stop_trigger_cols else NULL
  
  eligible <- build_new_user_trigger_cohort(
    comorbid_dt,
    patient,
    medication,
    treatment_drugs,
    estimand$population,
    start_trigger_col = specs$start_trigger_col,
    time_zero_col = specs$time_zero_col
  )
  eligible <- restrict_eligible_to_ehr_observation(
    eligible,
    ehr_observation_window
  )
  
  eligible <- add_stop_triggers(
    eligible,
    comorbid_dt,
    stop_trigger_cols = stop_trigger_cols
  )
  
  treatment_cohort <- prepare_post_time_zero_intervals(
    all_hazard_mod = all_hazard_mod,
    eligible = eligible
  )
  
  treatment_cohort <- mark_natural_treatment_exposure(
    treatment_cohort = treatment_cohort,
    medication = medication,
    eligible = eligible,
    drug_group = treatment_drugs
  )
  
  treatment_cohort[, treatment_log_hr_natural := fifelse(
    treatment_active_natural == 1L,
    treatment_start_log_hr,
    0
  )]
  
  treatment_cohort[, lnhr_affected_no_treatment :=
              get(specs$affected_hazard_column) - treatment_log_hr_natural]
  
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  
  no_stopping_worlds <- rbindlist(
    list(
      build_strategy_world(
        treatment_cohort,
        estimand$versions[1],
        "natural",
        treatment_cohort$treatment_active_natural,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort,
        estimand$versions[1],
        forced_drug_strategy,
        1L,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort,
        estimand$versions[1],
        forced_no_drug_strategy,
        0L,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      )
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  strategy_world_list <- list(no_stopping_worlds)
  
  if (has_stop_triggers) {
    with_stopping_worlds <- rbindlist(
      list(
        build_strategy_world(
          treatment_cohort,
          estimand$versions[2],
          "natural",
          treatment_cohort$treatment_active_natural,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort,
          estimand$versions[2],
          forced_drug_strategy,
          treatment_cohort$treatment_active_with_stopping,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort,
          estimand$versions[2],
          forced_no_drug_strategy,
          0L,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
    strategy_world_list <- c(strategy_world_list, list(with_stopping_worlds))
  }
  
  truth_worlds <- rbindlist(strategy_world_list, use.names = TRUE, fill = TRUE)
  
  list(
    eligible = eligible,
    treatment_cohort = treatment_cohort,
    truth_worlds = truth_worlds
  )
}

################################################################################
################################################################################

has_treatment_stop_triggers <- function(specs) {
  stop_cols <- specs$stop_trigger_cols
  
  !is.null(stop_cols) &&
    length(stop_cols) > 0L &&
    any(!is.na(stop_cols) & nzchar(stop_cols))
}

################################################################################
################################################################################

validate_treatment_specs <- function(specs) {
  required <- c(
    "drug_name",
    "population",
    "start_trigger_label",
    "strategy_context_label",
    "start_trigger_col",
    "time_zero_col",
    "affected_cause",
    "affected_hazard_column",
    "horizons"
  )
  
  missing <- setdiff(required, names(specs))
  if (length(missing) > 0) {
    stop(
      "Treatment specs missing required elements: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(specs$horizons)) {
    stop("Treatment specs field 'horizons' must be numeric.", call. = FALSE)
  }
  
  has_stop_labels <- "stop_trigger_labels" %in% names(specs) &&
    !is.null(specs$stop_trigger_labels)
  has_stop_cols <- "stop_trigger_cols" %in% names(specs) &&
    !is.null(specs$stop_trigger_cols)
  
  if (xor(has_stop_labels, has_stop_cols)) {
    stop(
      "Treatment specs should provide both stop_trigger_labels and ",
      "stop_trigger_cols, or neither.",
      call. = FALSE
    )
  }
  
  if (has_stop_labels &&
      length(specs$stop_trigger_labels) != length(specs$stop_trigger_cols)) {
    stop(
      "Treatment specs stop_trigger_labels and stop_trigger_cols must have ",
      "the same length.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

################################################################################
################################################################################

get_treatment_coefficients <- function(specs, prescription_log_hr_coefficients) {
  treatment_start_coef <- paste0("presc_start_", specs$drug_name)
  treatment_stop_coef <- paste0("presc_stop_", specs$drug_name)
  
  treatment_start_log_hr <- prescription_log_hr_coefficients[[treatment_start_coef]]
  treatment_stop_log_hr <- prescription_log_hr_coefficients[[treatment_stop_coef]]
  
  if (is.null(treatment_start_log_hr) || is.na(treatment_start_log_hr)) {
    stop("Could not find prescription coefficient '", treatment_start_coef, "'.",
         call. = FALSE)
  }
  
  if (is.null(treatment_stop_log_hr) || is.na(treatment_stop_log_hr)) {
    treatment_stop_log_hr <- -treatment_start_log_hr
  }
  
  list(
    treatment_start_coef = treatment_start_coef,
    treatment_stop_coef = treatment_stop_coef,
    treatment_start_log_hr = treatment_start_log_hr,
    treatment_stop_log_hr = treatment_stop_log_hr
  )
}


################################################################################
################################################################################

build_persistent_estimand <- function(specs){
  versions <- "persistent_no_stopping"
  
  if (has_treatment_stop_triggers(specs)) {
    versions <- c(versions, "persistent_with_stopping")
  }
  
  list(
    versions = versions,
    population = specs$population,
    time_zero = specs$time_zero_col,
    treatment = paste0("forced_", specs$drug_name, "_", specs$strategy_context_label),
    comparator = paste0("forced_no_", specs$drug_name, "_", specs$strategy_context_label),
    natural_strategy = "natural_as_simulated",
    outcome = "all_cause_survival",
    secondary_outcome = "non_communicable_survival",
    horizon = specs$horizons,
    primary_estimand = "RMST difference"
  )
}

################################################################################
################################################################################

build_persistent_treatment_effect_tables <- function(strategy_worlds, specs,
                                                     estimand,
                                                     treatment_start_log_hr,
                                                     treatment_stop_log_hr) {
  horizon_tables <- build_treatment_horizon_tables(
    strategy_worlds = strategy_worlds,
    horizons = estimand$horizon
  )
  
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  
  contrast_tables <- build_strategy_contrast_tables(
    horizon_patient = horizon_tables$patient,
    treatment_strategy = forced_drug_strategy,
    comparator_strategy = forced_no_drug_strategy
  )
  
  world_tables <- build_persistent_world_tables(
    horizon_strategy = horizon_tables$strategy,
    estimand = estimand,
    specs = specs
  )
  
  survival_curve_tables <- build_survival_curve_tables(
    strategy_worlds = strategy_worlds,
    max_horizon = max(estimand$horizon)
  )
  
  list(
    metadata = build_treatment_metadata_table(estimand),
    coefficients = build_treatment_coefficient_table(
      estimand = estimand,
      specs = specs,
      treatment_start_log_hr = treatment_start_log_hr,
      treatment_stop_log_hr = treatment_stop_log_hr
    ),
    horizon_patient = horizon_tables$patient,
    horizon_strategy = horizon_tables$strategy,
    horizon_world = world_tables$horizon_world,
    contrast_patient = contrast_tables$patient,
    contrast_population = contrast_tables$population,
    contrast_world_population = world_tables$contrast_world_population,
    survival_intervals = build_survival_interval_table(strategy_worlds),
    survival_curve_patient = survival_curve_tables$patient,
    survival_curve_mean = survival_curve_tables$mean
  )
}

################################################################################
################################################################################

run_persistent_treatment_effect <- function(patient, comorbid_dt, medication,
                                            drug_groups, all_hazard_mod,
                                            specs,
                                            prescription_log_hr_coefficients,
                                            ehr_observation_window = NULL) {
  validate_treatment_specs(specs)
  
  estimand <- build_persistent_estimand(specs)
  treatment_coef <- get_treatment_coefficients(
    specs = specs,
    prescription_log_hr_coefficients = prescription_log_hr_coefficients
  )
  
  treatment_persistent <- run_persistent_treatment_strategy(
    patient = patient,
    comorbid_dt = comorbid_dt,
    medication = medication,
    drug_groups = drug_groups,
    all_hazard_mod = all_hazard_mod,
    specs = specs,
    estimand = estimand,
    treatment_start_log_hr = treatment_coef$treatment_start_log_hr,
    ehr_observation_window = ehr_observation_window
  )
  
  truth_treatment_effects <- build_persistent_treatment_effect_tables(
    strategy_worlds = treatment_persistent$truth_worlds,
    specs = specs,
    estimand = estimand,
    treatment_start_log_hr = treatment_coef$treatment_start_log_hr,
    treatment_stop_log_hr = treatment_coef$treatment_stop_log_hr
  )
  
  result <- list(
    specs = specs,
    estimand = estimand,
    treatment_coef = treatment_coef,
    eligible = treatment_persistent$eligible,
    treatment_cohort = treatment_persistent$treatment_cohort,
    truth_worlds = treatment_persistent$truth_worlds,
    truth_treatment_effects = truth_treatment_effects
  )
  clean_treatment_run_output(result, specs)
}

################################################################################
# Fixed-course-once treatment strategies.
#
# A fixed course has one planned start and one planned end per eligible person.
# A delay of zero represents immediate treatment after time zero; a positive
# delay creates an untreated period before the same one-off course begins.

validate_fixed_course_specs <- function(specs) {
  required <- c(
    "drug_name", "population", "start_trigger_cols", "time_zero_col",
    "course_duration_days", "affected_cause", "affected_hazard_column",
    "horizons"
  )
  missing <- setdiff(required, names(specs))
  if (length(missing) > 0L) {
    stop(
      "Fixed-course specs missing required elements: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(specs$course_duration_days) != 1L ||
      is.na(specs$course_duration_days) ||
      specs$course_duration_days <= 0) {
    stop("course_duration_days must be one positive value.", call. = FALSE)
  }
  if (!is.null(specs$course_delay_days) &&
      (length(specs$course_delay_days) != 1L ||
       is.na(specs$course_delay_days) ||
       specs$course_delay_days < 0)) {
    stop("course_delay_days must be one non-negative value.", call. = FALSE)
  }
  invisible(TRUE)
}

# The two raw versions are needed to construct stopping and no-stopping worlds.
# Natural and never are later collapsed to one canonical copy each.
build_fixed_course_estimand <- function(specs) {
  versions <- "fixed_course_no_stopping"
  if (has_treatment_stop_triggers(specs)) {
    versions <- c(versions, "fixed_course_with_stopping")
  }
  list(
    versions = versions,
    population = specs$population,
    time_zero = specs$time_zero_col,
    treatment = paste0("fixed_course_", specs$drug_name),
    comparator = paste0("no_", specs$drug_name),
    natural_strategy = "natural_as_simulated",
    outcome = "all_cause_survival",
    secondary_outcome = "non_communicable_survival",
    horizon = specs$horizons,
    primary_estimand = "RMST difference"
  )
}

## Turn the fixed-course rule into three patient-level schedule dates.
## A stop during the delay gives actual_course_end <= course_start_date,
## which later prevents treatment from becoming active in the stopping world.
add_fixed_course_schedule <- function(eligible, specs) {
  course_delay_days <- if (is.null(specs$course_delay_days)) 0 else specs$course_delay_days
  eligible <- copy(eligible)
  
  eligible[, course_start_date := time_zero + course_delay_days]
  eligible[, planned_course_end :=
    course_start_date + specs$course_duration_days]
  eligible[, actual_course_end := pmin(
    planned_course_end,
    first_stop_date,
    enddate_init,
    na.rm = TRUE
  )]
  eligible[]
}

## Split every interval that contains a named person-level boundary date.
## Repeated calls support delayed starts, fixed ends, and future episodic work.
split_intervals_at_boundary <- function(treatment_cohort, boundary_col) {
  treatment_cohort <- copy(treatment_cohort)
  boundary <- treatment_cohort[[boundary_col]]
  split_rows <- !is.na(boundary) &
    treatment_cohort$date < boundary &
    boundary < treatment_cohort$date_end
  
  if (!any(split_rows)) {
    return(treatment_cohort[])
  }
  
  rows_to_split <- treatment_cohort[split_rows]
  rows_not_split <- treatment_cohort[!split_rows]
  before_boundary <- copy(rows_to_split)
  before_boundary[, date_end := get(boundary_col)]
  after_boundary <- copy(rows_to_split)
  after_boundary[, date := get(boundary_col)]
  
  rbindlist(
    list(rows_not_split, before_boundary, after_boundary),
    use.names = TRUE,
    fill = TRUE
  )
}

## Align every post-time-zero hazard interval with dates where fixed-course
## treatment can switch on or off, then derive strategy-specific flags.

prepare_fixed_course_intervals <- function(all_hazard_mod, eligible) {
  treatment_cohort <- copy(all_hazard_mod[patient_id %in% eligible$patient_id])
  treatment_cohort <- merge(treatment_cohort, eligible, by = "patient_id")
  treatment_cohort <- treatment_cohort[!is.na(date_end) & date_end > time_zero]
  treatment_cohort <- treatment_cohort[date < time_zero, date := time_zero]
  
  treatment_cohort <- split_intervals_at_boundary(
    treatment_cohort,
    "course_start_date"
  )
  treatment_cohort <- split_intervals_at_boundary(
    treatment_cohort,
    "actual_course_end"
  )
  treatment_cohort <- split_intervals_at_boundary(
    treatment_cohort,
    "planned_course_end"
  )
  
  treatment_cohort[, `:=`(
    interval_length = as.numeric(date_end - date) / 365.25,
    follow_start = as.numeric(date - time_zero) / 365.25,
    follow_end = as.numeric(date_end - time_zero) / 365.25
  )]
  
  treatment_cohort <- treatment_cohort[interval_length > 0]
  
  setorder(treatment_cohort, patient_id, follow_start, follow_end)
  treatment_cohort[, interval_id := .I]
  
  treatment_cohort[, treatment_active_fixed_course_with_stopping :=
                     as.integer(date >= course_start_date & date < actual_course_end)
  ]
  
  treatment_cohort[, treatment_active_fixed_course_no_stopping :=
                     as.integer(date >= course_start_date & date < planned_course_end)
  ]
  treatment_cohort[]
}

## Build the raw natural, forced-treatment, and never-treatment worlds from
## the neutral baseline. The stopping version is included only when stop
## triggers have been supplied in the specification.
run_fixed_course_treatment_strategy <- function(patient, comorbid_dt, medication,
                                                drug_groups, all_hazard_mod,
                                                specs, estimand,
                                                treatment_start_log_hr,
                                                ehr_observation_window = NULL) {
  treatment_drugs <- drug_groups[[specs$drug_name]]
  if (is.null(treatment_drugs)) {
    treatment_drugs <- specs$drug_name
  }
  stop_trigger_cols <- if (has_treatment_stop_triggers(specs)) {
    specs$stop_trigger_cols
  } else {
    NULL
  }
  
  eligible <- build_new_user_trigger_cohort(
    comorbid_dt = comorbid_dt,
    patient = patient,
    medication = medication,
    drug_group = treatment_drugs,
    population = estimand$population,
    start_trigger_cols = specs$start_trigger_cols,
    time_zero_col = specs$time_zero_col
  )
  eligible <- restrict_eligible_to_ehr_observation(
    eligible,
    ehr_observation_window
  )
  eligible <- add_stop_triggers(eligible, comorbid_dt, stop_trigger_cols)
  eligible <- add_fixed_course_schedule(eligible, specs)
  
  treatment_cohort <- prepare_fixed_course_intervals(all_hazard_mod, eligible)
  treatment_cohort <- mark_natural_treatment_exposure(
    treatment_cohort = treatment_cohort,
    medication = medication,
    eligible = eligible,
    drug_group = treatment_drugs
  )
  treatment_cohort[, treatment_log_hr_natural := fifelse(
    treatment_active_natural == 1L,
    treatment_start_log_hr,
    0
  )]
  treatment_cohort[, lnhr_affected_no_treatment :=
    get(specs$affected_hazard_column) - treatment_log_hr_natural]
  
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  no_stopping_worlds <- rbindlist(
    list(
      build_strategy_world(
        treatment_cohort, estimand$versions[1], "natural",
        treatment_cohort$treatment_active_natural,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort, estimand$versions[1], forced_drug_strategy,
        treatment_cohort$treatment_active_fixed_course_no_stopping,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort, estimand$versions[1], forced_no_drug_strategy,
        0L,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      )
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  strategy_worlds <- list(no_stopping_worlds)
  if (has_treatment_stop_triggers(specs)) {
    with_stopping_worlds <- rbindlist(
      list(
        build_strategy_world(
          treatment_cohort, estimand$versions[2], "natural",
          treatment_cohort$treatment_active_natural,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort, estimand$versions[2], forced_drug_strategy,
          treatment_cohort$treatment_active_fixed_course_with_stopping,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort, estimand$versions[2], forced_no_drug_strategy,
          0L,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
    strategy_worlds <- c(strategy_worlds, list(with_stopping_worlds))
  }
  
  list(
    eligible = eligible,
    treatment_cohort = treatment_cohort,
    truth_worlds = rbindlist(strategy_worlds, use.names = TRUE, fill = TRUE)
  )
}

## Map the raw construction versions to the four user-facing strategy worlds.
## The raw natural and never rows in the stopping version are duplicates and
## are deliberately omitted here.
build_fixed_course_world_tables <- function(horizon_patient, estimand, specs) {
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  has_stop_world <- length(estimand$versions) >= 2L
  
  horizon_patient_world <- copy(horizon_patient)
  horizon_patient_world[, strategy_world := fcase(
    truth_version == estimand$versions[1] & strategy == "natural", "natural",
    truth_version == estimand$versions[1] &
      strategy == forced_no_drug_strategy, "never",
    truth_version == estimand$versions[1] &
      strategy == forced_drug_strategy, "fixed_course_no_stop",
    has_stop_world & truth_version == estimand$versions[2] &
      strategy == forced_drug_strategy, "fixed_course_stop",
    default = NA_character_
  )]
  horizon_patient_world <- horizon_patient_world[!is.na(strategy_world)]
  
  horizon_world <- horizon_patient_world[
    ,
    .(
      n_patients = uniqueN(patient_id),
      n_complete = sum(complete_horizon),
      mean_survival = mean(survival, na.rm = TRUE),
      mean_risk = mean(risk, na.rm = TRUE),
      mean_rmst = mean(rmst, na.rm = TRUE)
    ),
    by = .(truth_version, horizon_years, strategy_world, strategy)
  ]
  
  horizon_patient_wide <- dcast(
    horizon_patient_world,
    patient_id + horizon_years + complete_horizon ~ strategy_world,
    value.var = c("survival", "risk", "rmst")
  )
  
  contrast_world_patient <- horizon_patient_wide[, .(
    patient_id,
    horizon_years,
    complete_horizon,
    survival_diff_natural_vs_never = survival_natural - survival_never,
    survival_diff_fixed_course_no_stop_vs_never =
      survival_fixed_course_no_stop - survival_never,
    risk_diff_natural_vs_never = risk_natural - risk_never,
    risk_diff_fixed_course_no_stop_vs_never =
      risk_fixed_course_no_stop - risk_never,
    rmst_diff_natural_vs_never = rmst_natural - rmst_never,
    rmst_diff_fixed_course_no_stop_vs_never =
      rmst_fixed_course_no_stop - rmst_never
  )]
  
  if ("survival_fixed_course_stop" %in% names(horizon_patient_wide)) {
    contrast_world_patient[, `:=`(
      survival_diff_fixed_course_stop_vs_never =
        horizon_patient_wide$survival_fixed_course_stop -
        horizon_patient_wide$survival_never,
      risk_diff_fixed_course_stop_vs_never =
        horizon_patient_wide$risk_fixed_course_stop -
        horizon_patient_wide$risk_never,
      rmst_diff_fixed_course_stop_vs_never =
        horizon_patient_wide$rmst_fixed_course_stop -
        horizon_patient_wide$rmst_never
    )]
  }
  
  contrast_cols <- setdiff(
    names(contrast_world_patient),
    c("patient_id", "horizon_years", "complete_horizon")
  )
  contrast_world_population <- contrast_world_patient[
    ,
    c(
      list(
        n_patients = uniqueN(patient_id),
        n_complete = sum(complete_horizon)
      ),
      lapply(.SD, mean, na.rm = TRUE)
    ),
    by = horizon_years,
    .SDcols = contrast_cols
  ]
  setnames(
    contrast_world_population,
    contrast_cols,
    paste0("mean_", contrast_cols)
  )
  
  rmst_cols <- grep(
    "^mean_rmst_diff_",
    names(contrast_world_population),
    value = TRUE
  )
  for (rmst_col in rmst_cols) {
    rmst_days_col <- sub(
      "^mean_rmst_diff_",
      "mean_rmst_days_diff_",
      rmst_col
    )
    contrast_world_population[, (rmst_days_col) := 365.25 * get(rmst_col)]
  }
  
  list(
    horizon_patient = horizon_patient_world,
    horizon_world = horizon_world,
    contrast_patient = contrast_world_patient,
    contrast_population = contrast_world_population
  )
}

## Reuse the shared horizon, contrast, and survival-curve engines. Only the
## exposure schedule and the canonical fixed-course labels are strategy-specific.
build_fixed_course_treatment_effect_tables <- function(strategy_worlds, specs,
                                                       estimand) {
  horizon_tables <- build_treatment_horizon_tables(
    strategy_worlds,
    estimand$horizon
  )
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  raw_contrast_tables <- build_strategy_contrast_tables(
    horizon_tables$patient,
    forced_drug_strategy,
    forced_no_drug_strategy
  )
  world_tables <- build_fixed_course_world_tables(
    horizon_tables$patient,
    estimand,
    specs
  )
  survival_curve_tables <- build_survival_curve_tables(
    strategy_worlds,
    max(estimand$horizon)
  )
  
  list(
    horizon_patient = horizon_tables$patient,
    horizon_strategy = horizon_tables$strategy,
    horizon_world = world_tables$horizon_world,
    contrast_patient = raw_contrast_tables$patient,
    contrast_population = raw_contrast_tables$population,
    contrast_world_patient = world_tables$contrast_patient,
    contrast_world_population = world_tables$contrast_population,
    survival_intervals = build_survival_interval_table(strategy_worlds),
    survival_curve_patient = survival_curve_tables$patient,
    survival_curve_mean = survival_curve_tables$mean
  )
}

## Main entry point used by P7 scripts. It returns the construction objects for
## debugging as well as the presentation-ready survival and contrast tables.
run_fixed_course_treatment_effect <- function(patient, comorbid_dt, medication,
                                              drug_groups, all_hazard_mod,
                                              specs,
                                              prescription_log_hr_coefficients,
                                              ehr_observation_window = NULL) {
  validate_fixed_course_specs(specs)
  estimand <- build_fixed_course_estimand(specs)
  treatment_coef <- get_treatment_coefficients(
    specs,
    prescription_log_hr_coefficients
  )
  treatment_fixed_course <- run_fixed_course_treatment_strategy(
    patient = patient,
    comorbid_dt = comorbid_dt,
    medication = medication,
    drug_groups = drug_groups,
    all_hazard_mod = all_hazard_mod,
    specs = specs,
    estimand = estimand,
    treatment_start_log_hr = treatment_coef$treatment_start_log_hr,
    ehr_observation_window = ehr_observation_window
  )
  truth_treatment_effects <- build_fixed_course_treatment_effect_tables(
    strategy_worlds = treatment_fixed_course$truth_worlds,
    specs = specs,
    estimand = estimand
  )
  
  result <- list(
    specs = specs,
    estimand = estimand,
    treatment_coef = treatment_coef,
    eligible = treatment_fixed_course$eligible,
    treatment_cohort = treatment_fixed_course$treatment_cohort,
    truth_worlds = treatment_fixed_course$truth_worlds,
    truth_treatment_effects = truth_treatment_effects
  )
  clean_treatment_run_output(result, specs)
}

################################################################################
## Episodic Course Strategies

validate_episodic_specs <- function(specs) {
  required <- c(
    "strategy_type", "drug_name", "population", "start_trigger_cols",
    "time_zero_col", "course_duration_days", "affected_cause",
    "affected_hazard_column", "horizons"
  )
  missing <- setdiff(required, names(specs))
  if (length(missing) > 0L) {
    stop(
      "Episodic specs missing required elements: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(specs$course_duration_days) != 1L ||
      is.na(specs$course_duration_days) ||
      specs$course_duration_days <= 0) {
    stop("course_duration_days must be one positive value.", call. = FALSE)
  }
  if (!is.null(specs$course_delay_days) &&
      (length(specs$course_delay_days) != 1L ||
       is.na(specs$course_delay_days) ||
       specs$course_delay_days < 0)) {
    stop("course_delay_days must be one non-negative value.", call. = FALSE)
  }
  invisible(TRUE)
}

get_treatment_drugs <- function(drug_groups, specs) {
  treatment_drugs <- drug_groups[[specs$drug_name]]
  if (is.null(treatment_drugs)) {
    treatment_drugs <- specs$drug_name
  }
  treatment_drugs
}

build_episodic_estimand <- function(specs) {
  restart_after_stop <- isTRUE(specs$restart_after_stop)
  versions <- if (restart_after_stop) {
    "episodic_restart_no_stopping"
  } else {
    "episodic_no_stopping"
  }
  if (has_treatment_stop_triggers(specs)) {
    versions <- c(
      versions,
      if (restart_after_stop) {
        "episodic_restart_with_stopping"
      } else {
        "episodic_with_stopping"
      }
    )
  }
  
  list(
    versions = versions,
    population = specs$population,
    time_zero = specs$time_zero_col,
    treatment = paste0("episodic_course_", specs$drug_name),
    comparator = paste0("no_", specs$drug_name),
    natural_strategy = "natural_as_simulated",
    outcome = "all_cause_survival",
    secondary_outcome = "non_communicable_survival",
    horizon = specs$horizons,
    primary_estimand = "RMST difference"
  )
}

build_episodic_trigger_table <- function(comorbid_dt, specs) {
  trigger_cols <- intersect(specs$start_trigger_cols, names(comorbid_dt))
  if (length(trigger_cols) == 0L) {
    stop("Requested episodic start triggers not found in comorbid_dt.",
         call. = FALSE)
  }
  
  trigger_wide <- unique(
    comorbid_dt[, c("patient_id", trigger_cols), with = FALSE]
  )
  triggers <- melt(
    trigger_wide,
    id.vars = "patient_id",
    measure.vars = trigger_cols,
    variable.name = "trigger_event",
    value.name = "trigger_date",
    na.rm = TRUE
  )
  setorder(triggers, patient_id, trigger_date, trigger_event)
  triggers[]
}

build_episodic_trigger_inputs <- function(patient, comorbid_dt, medication,
                                          drug_groups, specs,
                                          ehr_observation_window = NULL) {
  treatment_drugs <- get_treatment_drugs(drug_groups, specs)
  triggers <- build_episodic_trigger_table(comorbid_dt, specs)
  stop_trigger_cols <- if (has_treatment_stop_triggers(specs)) {
    specs$stop_trigger_cols
  } else {
    NULL
  }
  
  eligible <- build_new_user_trigger_cohort(
    comorbid_dt = comorbid_dt,
    patient = patient,
    medication = medication,
    drug_group = treatment_drugs,
    population = specs$population,
    start_trigger_cols = specs$start_trigger_cols,
    time_zero_col = specs$time_zero_col
  )
  if ("trigger_event" %in% names(eligible)) {
    setnames(eligible, "trigger_event", "first_trigger_event")
  }
  eligible <- restrict_eligible_to_ehr_observation(
    eligible,
    ehr_observation_window
  )
  eligible <- add_stop_triggers(eligible, comorbid_dt, stop_trigger_cols)
  
  episode_triggers <- merge(
    triggers,
    eligible[, .(
      patient_id,
      time_zero,
      enddate_init,
      first_stop_date,
      first_stop_event
    )],
    by = "patient_id"
  )
  episode_triggers <- episode_triggers[
    trigger_date >= time_zero & trigger_date < enddate_init
  ]
  setorder(episode_triggers, patient_id, trigger_date, trigger_event)
  episode_triggers[, episode_id := seq_len(.N), by = patient_id]
  
  list(
    treatment_drugs = treatment_drugs,
    triggers = triggers,
    eligible = eligible,
    episode_triggers = episode_triggers
  )
}

build_episode_stop_table <- function(episode_triggers, comorbid_dt, specs) {
  stop_cols <- intersect(specs$stop_trigger_cols, names(comorbid_dt))
  if (length(stop_cols) == 0L || nrow(episode_triggers) == 0L) {
    return(
      episode_triggers[, .(
        patient_id,
        episode_id,
        episode_stop_date = as.Date(NA),
        episode_stop_event = NA_character_
      )]
    )
  }
  
  stop_wide <- unique(comorbid_dt[, c("patient_id", stop_cols), with = FALSE])
  stop_long <- melt(
    stop_wide,
    id.vars = "patient_id",
    measure.vars = stop_cols,
    variable.name = "episode_stop_event",
    value.name = "episode_stop_date",
    na.rm = TRUE
  )
  
  episode_stop <- merge(
    episode_triggers[, .(
      patient_id,
      episode_id,
      course_start_date,
      planned_course_end
    )],
    stop_long,
    by = "patient_id",
    allow.cartesian = TRUE
  )
  episode_stop <- episode_stop[
    episode_stop_date >= course_start_date &
      episode_stop_date < planned_course_end
  ]
  
  if (nrow(episode_stop) == 0L) {
    return(
      episode_triggers[, .(
        patient_id,
        episode_id,
        episode_stop_date = as.Date(NA),
        episode_stop_event = NA_character_
      )]
    )
  }
  
  setorder(episode_stop, patient_id, episode_id, episode_stop_date,
           episode_stop_event)
  episode_stop[, .SD[1], by = .(patient_id, episode_id)]
}

add_episodic_course_windows <- function(episode_triggers, comorbid_dt, specs) {
  course_delay_days <- if (is.null(specs$course_delay_days)) {
    0
  } else {
    specs$course_delay_days
  }
  episode_triggers <- copy(episode_triggers)
  
  episode_triggers[, course_start_date := trigger_date + course_delay_days]
  episode_triggers[, planned_course_end :=
    course_start_date + specs$course_duration_days]
  
  if (isTRUE(specs$restart_after_stop)) {
    episode_stop <- build_episode_stop_table(
      episode_triggers,
      comorbid_dt,
      specs
    )
    episode_triggers <- merge(
      episode_triggers,
      episode_stop[, .(
        patient_id,
        episode_id,
        episode_stop_date,
        episode_stop_event
      )],
      by = c("patient_id", "episode_id"),
      all.x = TRUE
    )
  } else {
    episode_triggers[, `:=`(
      episode_stop_date = first_stop_date,
      episode_stop_event = first_stop_event
    )]
  }
  
  episode_triggers[, actual_course_end := pmin(
    planned_course_end,
    episode_stop_date,
    enddate_init,
    na.rm = TRUE
  )]
  setorder(episode_triggers, patient_id, trigger_date, trigger_event)
  episode_triggers[]
}

split_intervals_at_episode_boundaries <- function(treatment_cohort,
                                                  episode_triggers) {
  boundary_table <- melt(
    episode_triggers[, .(
      patient_id,
      time_zero,
      enddate_init,
      course_start_date,
      planned_course_end,
      actual_course_end
    )],
    id.vars = c("patient_id", "time_zero", "enddate_init"),
    measure.vars = c(
      "course_start_date",
      "planned_course_end",
      "actual_course_end"
    ),
    variable.name = "boundary_type",
    value.name = "boundary_date",
    na.rm = TRUE
  )
  
  boundary_table <- unique(
    boundary_table[
      boundary_date > time_zero & boundary_date < enddate_init,
      .(patient_id, boundary_date)
    ]
  )
  
  treatment_cohort[, base_interval_id := .I]
  boundaries_in_intervals <- merge(
    treatment_cohort[, .(base_interval_id, patient_id, date, date_end)],
    boundary_table,
    by = "patient_id",
    allow.cartesian = TRUE
  )
  boundaries_in_intervals <- boundaries_in_intervals[
    date < boundary_date & boundary_date < date_end
  ]
  
  split_points <- boundaries_in_intervals[
    ,
    .(split_points = list(sort(unique(boundary_date)))),
    by = base_interval_id
  ]
  
  if (nrow(split_points) > 0L) {
    rows_to_split <- merge(
      treatment_cohort[base_interval_id %in% split_points$base_interval_id],
      split_points,
      by = "base_interval_id"
    )
    
    split_rows <- rows_to_split[
      ,
      {
        cuts <- c(date[1], split_points[[1]], date_end[1])
        out <- .SD[rep(1L, length(cuts) - 1L)]
        out[, `:=`(
          date = cuts[-length(cuts)],
          date_end = cuts[-1L]
        )]
        out
      },
      by = base_interval_id
    ]
    
    treatment_cohort <- rbindlist(
      list(
        treatment_cohort[!base_interval_id %in% split_points$base_interval_id],
        split_rows
      ),
      use.names = TRUE,
      fill = TRUE
    )
  }
  
  drop_cols <- intersect(
    c("base_interval_id", "split_points"),
    names(treatment_cohort)
  )
  if (length(drop_cols) > 0L) {
    treatment_cohort[, (drop_cols) := NULL]
  }
  
  treatment_cohort[]
}

prepare_episodic_treatment_intervals <- function(all_hazard_mod, eligible,
                                                 episode_triggers) {
  treatment_cohort <- copy(all_hazard_mod[patient_id %in% eligible$patient_id])
  treatment_cohort <- merge(treatment_cohort, eligible, by = "patient_id")
  treatment_cohort <- treatment_cohort[!is.na(date_end) & date_end > time_zero]
  treatment_cohort[date < time_zero, date := time_zero]
  
  treatment_cohort <- split_intervals_at_episode_boundaries(
    treatment_cohort,
    episode_triggers
  )
  
  treatment_cohort[, `:=`(
    interval_length = as.numeric(date_end - date) / 365.25,
    follow_start = as.numeric(date - time_zero) / 365.25,
    follow_end = as.numeric(date_end - time_zero) / 365.25
  )]
  treatment_cohort <- treatment_cohort[interval_length > 0]
  setorder(treatment_cohort, patient_id, follow_start, follow_end)
  treatment_cohort[, interval_id := .I]
  
  episode_overlap <- merge(
    treatment_cohort[, .(interval_id, patient_id, date, date_end)],
    episode_triggers[, .(
      patient_id,
      episode_id,
      course_start_date,
      planned_course_end,
      actual_course_end
    )],
    by = "patient_id",
    allow.cartesian = TRUE
  )
  
  active_no_stop_ids <- episode_overlap[
    date < planned_course_end & date_end > course_start_date,
    unique(interval_id)
  ]
  active_stop_ids <- episode_overlap[
    date < actual_course_end & date_end > course_start_date,
    unique(interval_id)
  ]
  
  treatment_cohort[, treatment_active_episodic_no_stopping :=
    as.integer(interval_id %in% active_no_stop_ids)]
  treatment_cohort[, treatment_active_episodic_with_stopping :=
    as.integer(interval_id %in% active_stop_ids)]
  treatment_cohort[]
}

run_episodic_treatment_strategy <- function(patient, comorbid_dt, medication,
                                            drug_groups, all_hazard_mod,
                                            specs, estimand,
                                            treatment_start_log_hr,
                                            ehr_observation_window = NULL) {
  trigger_inputs <- build_episodic_trigger_inputs(
    patient = patient,
    comorbid_dt = comorbid_dt,
    medication = medication,
    drug_groups = drug_groups,
    specs = specs,
    ehr_observation_window = ehr_observation_window
  )
  episode_triggers <- add_episodic_course_windows(
    trigger_inputs$episode_triggers,
    comorbid_dt,
    specs
  )
  treatment_cohort <- prepare_episodic_treatment_intervals(
    all_hazard_mod = all_hazard_mod,
    eligible = trigger_inputs$eligible,
    episode_triggers = episode_triggers
  )
  treatment_cohort <- mark_natural_treatment_exposure(
    treatment_cohort = treatment_cohort,
    medication = medication,
    eligible = trigger_inputs$eligible,
    drug_group = trigger_inputs$treatment_drugs
  )
  treatment_cohort[, treatment_log_hr_natural := fifelse(
    treatment_active_natural == 1L,
    treatment_start_log_hr,
    0
  )]
  treatment_cohort[, lnhr_affected_no_treatment :=
    get(specs$affected_hazard_column) - treatment_log_hr_natural]
  
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  no_stopping_worlds <- rbindlist(
    list(
      build_strategy_world(
        treatment_cohort, estimand$versions[1], "natural",
        treatment_cohort$treatment_active_natural,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort, estimand$versions[1], forced_drug_strategy,
        treatment_cohort$treatment_active_episodic_no_stopping,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      ),
      build_strategy_world(
        treatment_cohort, estimand$versions[1], forced_no_drug_strategy,
        0L,
        treatment_start_log_hr,
        affected_hazard_column = specs$affected_hazard_column
      )
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  strategy_worlds <- list(no_stopping_worlds)
  if (has_treatment_stop_triggers(specs)) {
    with_stopping_worlds <- rbindlist(
      list(
        build_strategy_world(
          treatment_cohort, estimand$versions[2], "natural",
          treatment_cohort$treatment_active_natural,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort, estimand$versions[2], forced_drug_strategy,
          treatment_cohort$treatment_active_episodic_with_stopping,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        ),
        build_strategy_world(
          treatment_cohort, estimand$versions[2], forced_no_drug_strategy,
          0L,
          treatment_start_log_hr,
          affected_hazard_column = specs$affected_hazard_column
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
    strategy_worlds <- c(strategy_worlds, list(with_stopping_worlds))
  }
  
  list(
    treatment_drugs = trigger_inputs$treatment_drugs,
    triggers = trigger_inputs$triggers,
    eligible = trigger_inputs$eligible,
    episode_triggers = episode_triggers,
    treatment_cohort = treatment_cohort,
    truth_worlds = rbindlist(strategy_worlds, use.names = TRUE, fill = TRUE)
  )
}

build_episodic_world_tables <- function(horizon_patient, estimand, specs) {
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  has_stop_world <- length(estimand$versions) >= 2L
  
  horizon_patient_world <- copy(horizon_patient)
  horizon_patient_world[, strategy_world := fcase(
    truth_version == estimand$versions[1] & strategy == "natural", "natural",
    truth_version == estimand$versions[1] &
      strategy == forced_no_drug_strategy, "never",
    truth_version == estimand$versions[1] &
      strategy == forced_drug_strategy, "episodic_no_stop",
    has_stop_world & truth_version == estimand$versions[2] &
      strategy == forced_drug_strategy, "episodic_stop",
    default = NA_character_
  )]
  horizon_patient_world <- horizon_patient_world[!is.na(strategy_world)]
  
  horizon_world <- horizon_patient_world[
    ,
    .(
      n_patients = uniqueN(patient_id),
      n_complete = sum(complete_horizon),
      mean_survival = mean(survival, na.rm = TRUE),
      mean_risk = mean(risk, na.rm = TRUE),
      mean_rmst = mean(rmst, na.rm = TRUE)
    ),
    by = .(truth_version, horizon_years, strategy_world, strategy)
  ]
  
  horizon_patient_wide <- dcast(
    horizon_patient_world,
    patient_id + horizon_years + complete_horizon ~ strategy_world,
    value.var = c("survival", "risk", "rmst")
  )
  
  contrast_world_patient <- horizon_patient_wide[, .(
    patient_id,
    horizon_years,
    complete_horizon,
    survival_diff_natural_vs_never = survival_natural - survival_never,
    survival_diff_episodic_no_stop_vs_never =
      survival_episodic_no_stop - survival_never,
    risk_diff_natural_vs_never = risk_natural - risk_never,
    risk_diff_episodic_no_stop_vs_never =
      risk_episodic_no_stop - risk_never,
    rmst_diff_natural_vs_never = rmst_natural - rmst_never,
    rmst_diff_episodic_no_stop_vs_never =
      rmst_episodic_no_stop - rmst_never
  )]
  
  if ("survival_episodic_stop" %in% names(horizon_patient_wide)) {
    contrast_world_patient[, `:=`(
      survival_diff_episodic_stop_vs_never =
        horizon_patient_wide$survival_episodic_stop -
        horizon_patient_wide$survival_never,
      risk_diff_episodic_stop_vs_never =
        horizon_patient_wide$risk_episodic_stop -
        horizon_patient_wide$risk_never,
      rmst_diff_episodic_stop_vs_never =
        horizon_patient_wide$rmst_episodic_stop -
        horizon_patient_wide$rmst_never
    )]
  }
  
  contrast_cols <- setdiff(
    names(contrast_world_patient),
    c("patient_id", "horizon_years", "complete_horizon")
  )
  contrast_world_population <- contrast_world_patient[
    ,
    c(
      list(
        n_patients = uniqueN(patient_id),
        n_complete = sum(complete_horizon)
      ),
      lapply(.SD, mean, na.rm = TRUE)
    ),
    by = horizon_years,
    .SDcols = contrast_cols
  ]
  setnames(
    contrast_world_population,
    contrast_cols,
    paste0("mean_", contrast_cols)
  )
  
  rmst_cols <- grep(
    "^mean_rmst_diff_",
    names(contrast_world_population),
    value = TRUE
  )
  for (rmst_col in rmst_cols) {
    rmst_days_col <- sub(
      "^mean_rmst_diff_",
      "mean_rmst_days_diff_",
      rmst_col
    )
    contrast_world_population[, (rmst_days_col) := 365.25 * get(rmst_col)]
  }
  
  list(
    horizon_patient = horizon_patient_world,
    horizon_world = horizon_world,
    contrast_patient = contrast_world_patient,
    contrast_population = contrast_world_population
  )
}

build_episodic_treatment_effect_tables <- function(strategy_worlds, specs,
                                                   estimand,
                                                   treatment_start_log_hr,
                                                   treatment_stop_log_hr) {
  horizon_tables <- build_treatment_horizon_tables(
    strategy_worlds,
    estimand$horizon
  )
  forced_drug_strategy <- paste0("forced_", specs$drug_name)
  forced_no_drug_strategy <- paste0("forced_no_", specs$drug_name)
  raw_contrast_tables <- build_strategy_contrast_tables(
    horizon_tables$patient,
    forced_drug_strategy,
    forced_no_drug_strategy
  )
  world_tables <- build_episodic_world_tables(
    horizon_tables$patient,
    estimand,
    specs
  )
  survival_curve_tables <- build_survival_curve_tables(
    strategy_worlds,
    max(estimand$horizon)
  )
  
  list(
    metadata = build_treatment_metadata_table(estimand),
    coefficients = build_treatment_coefficient_table(
      estimand = estimand,
      specs = specs,
      treatment_start_log_hr = treatment_start_log_hr,
      treatment_stop_log_hr = treatment_stop_log_hr
    ),
    horizon_patient = horizon_tables$patient,
    horizon_strategy = horizon_tables$strategy,
    horizon_world = world_tables$horizon_world,
    contrast_patient = raw_contrast_tables$patient,
    contrast_population = raw_contrast_tables$population,
    contrast_world_patient = world_tables$contrast_patient,
    contrast_world_population = world_tables$contrast_population,
    survival_intervals = build_survival_interval_table(strategy_worlds),
    survival_curve_patient = survival_curve_tables$patient,
    survival_curve_mean = survival_curve_tables$mean
  )
}

run_episodic_treatment_effect <- function(patient, comorbid_dt, medication,
                                          drug_groups, all_hazard_mod,
                                          specs,
                                          prescription_log_hr_coefficients,
                                          ehr_observation_window = NULL) {
  validate_episodic_specs(specs)
  estimand <- build_episodic_estimand(specs)
  treatment_coef <- get_treatment_coefficients(
    specs,
    prescription_log_hr_coefficients
  )
  episodic_strategy <- run_episodic_treatment_strategy(
    patient = patient,
    comorbid_dt = comorbid_dt,
    medication = medication,
    drug_groups = drug_groups,
    all_hazard_mod = all_hazard_mod,
    specs = specs,
    estimand = estimand,
    treatment_start_log_hr = treatment_coef$treatment_start_log_hr,
    ehr_observation_window = ehr_observation_window
  )
  truth_treatment_effects <- build_episodic_treatment_effect_tables(
    strategy_worlds = episodic_strategy$truth_worlds,
    specs = specs,
    estimand = estimand,
    treatment_start_log_hr = treatment_coef$treatment_start_log_hr,
    treatment_stop_log_hr = treatment_coef$treatment_stop_log_hr
  )
  
  result <- list(
    specs = specs,
    estimand = estimand,
    treatment_coef = treatment_coef,
    treatment_drugs = episodic_strategy$treatment_drugs,
    triggers = episodic_strategy$triggers,
    eligible = episodic_strategy$eligible,
    episode_triggers = episodic_strategy$episode_triggers,
    treatment_cohort = episodic_strategy$treatment_cohort,
    truth_worlds = episodic_strategy$truth_worlds,
    truth_treatment_effects = truth_treatment_effects
  )
  clean_treatment_run_output(result, specs)
}
