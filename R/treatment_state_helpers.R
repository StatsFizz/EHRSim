.validate_treatment_inputs <- function(pracid, gp_rel_size, seed, specs,
                                       strategy_family) {
  .validate_simulation_inputs(pracid, gp_rel_size, seed)

  if (!is.list(specs) || is.data.frame(specs)) {
    stop("specs must be a named list.", call. = FALSE)
  }
  if (length(specs) == 0L || is.null(names(specs)) ||
      anyNA(names(specs)) || any(!nzchar(names(specs)))) {
    stop("specs must be a non-empty list with a name for every element.",
         call. = FALSE)
  }
  if (anyDuplicated(names(specs))) {
    duplicated_names <- unique(names(specs)[duplicated(names(specs))])
    stop(
      "specs contains duplicated elements: ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }

  required <- switch(
    strategy_family,
    persistent = c(
      "drug_name", "population", "start_trigger_label",
      "strategy_context_label", "start_trigger_col", "time_zero_col",
      "affected_cause", "affected_hazard_column", "horizons"
    ),
    fixed = c(
      "strategy_type", "drug_name", "population", "start_trigger_cols",
      "time_zero_col", "course_duration_days", "affected_cause",
      "affected_hazard_column", "horizons"
    ),
    episodic = c(
      "strategy_type", "drug_name", "population", "start_trigger_cols",
      "time_zero_col", "course_duration_days", "affected_cause",
      "affected_hazard_column", "horizons"
    ),
    stop("Unknown treatment strategy family: ", strategy_family,
         call. = FALSE)
  )

  missing_elements <- setdiff(required, names(specs))
  if (length(missing_elements) > 0L) {
    stop(
      strategy_family,
      " treatment specs missing required elements: ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }

  empty_elements <- required[vapply(
    specs[required],
    function(value) {
      is.null(value) || length(value) == 0L ||
        (is.atomic(value) && all(is.na(value))) ||
        (is.character(value) && all(!nzchar(value)))
    },
    logical(1)
  )]
  if (length(empty_elements) > 0L) {
    stop(
      strategy_family,
      " treatment specs contain empty required elements: ",
      paste(empty_elements, collapse = ", "),
      call. = FALSE
    )
  }

  scalar_character_fields <- intersect(
    c(
      "strategy_type", "drug_name", "population", "start_trigger_label",
      "strategy_context_label", "start_trigger_col", "time_zero_col",
      "affected_cause", "affected_hazard_column"
    ),
    required
  )
  invalid_character_fields <- scalar_character_fields[!vapply(
    specs[scalar_character_fields],
    function(value) {
      is.character(value) && length(value) == 1L &&
        !is.na(value) && nzchar(value)
    },
    logical(1)
  )]
  if (length(invalid_character_fields) > 0L) {
    stop(
      "Treatment specs fields must each contain one non-empty character value: ",
      paste(invalid_character_fields, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_strategy_types <- switch(
    strategy_family,
    fixed = c(
      "fixed_course_once",
      "fixed_course_delayed",
      "multi_trigger_fixed_course"
    ),
    episodic = c(
      "episodic_course",
      "episodic_triggered",
      "episodic_course_one_stop",
      "episodic_course_restart"
    ),
    NULL
  )
  if (!is.null(allowed_strategy_types) &&
      !specs$strategy_type %in% allowed_strategy_types) {
    stop(
      "specs$strategy_type is not valid for the ",
      strategy_family,
      " treatment wrapper. Expected one of: ",
      paste(allowed_strategy_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if ("start_trigger_cols" %in% required &&
      (!is.character(specs$start_trigger_cols) ||
       anyNA(specs$start_trigger_cols) ||
       any(!nzchar(specs$start_trigger_cols)))) {
    stop("start_trigger_cols must contain non-empty column names.",
         call. = FALSE)
  }

  if (!is.numeric(specs$horizons) || length(specs$horizons) == 0L ||
      anyNA(specs$horizons) || any(!is.finite(specs$horizons)) ||
      any(specs$horizons <= 0)) {
    stop("horizons must contain finite positive numeric values.",
         call. = FALSE)
  }

  if (strategy_family %in% c("fixed", "episodic")) {
    .validate_single_number(
      specs$course_duration_days,
      "course_duration_days",
      lower_bound = 0
    )
    if (specs$course_duration_days == 0) {
      stop("course_duration_days must be greater than 0.", call. = FALSE)
    }

    if (!is.null(specs$course_delay_days)) {
      .validate_single_number(
        specs$course_delay_days,
        "course_delay_days",
        lower_bound = 0
      )
    }
  }

  if (!is.null(specs$restart_after_stop) &&
      (!is.logical(specs$restart_after_stop) ||
       length(specs$restart_after_stop) != 1L ||
       is.na(specs$restart_after_stop))) {
    stop("restart_after_stop must be TRUE or FALSE.", call. = FALSE)
  }

  has_stop_labels <- !is.null(specs$stop_trigger_labels)
  has_stop_cols <- !is.null(specs$stop_trigger_cols)
  if (xor(has_stop_labels, has_stop_cols)) {
    stop(
      "Treatment specs must provide both stop_trigger_labels and ",
      "stop_trigger_cols, or neither.",
      call. = FALSE
    )
  }
  if (has_stop_labels) {
    if (!is.character(specs$stop_trigger_labels) ||
        !is.character(specs$stop_trigger_cols) ||
        anyNA(specs$stop_trigger_labels) || anyNA(specs$stop_trigger_cols) ||
        any(!nzchar(specs$stop_trigger_labels)) ||
        any(!nzchar(specs$stop_trigger_cols))) {
      stop("Treatment stop triggers must contain non-empty character values.",
           call. = FALSE)
    }
    if (length(specs$stop_trigger_labels) != length(specs$stop_trigger_cols)) {
      stop(
        "stop_trigger_labels and stop_trigger_cols must have the same length.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

.validate_treatment_state <- function(state, runner_name, specs) {
  if (!is.environment(state)) {
    stop("The simulation state must be an environment.", call. = FALSE)
  }

  required_objects <- c(
    "patient", "comorbid_dt", "medication", "drug_groups",
    "all_hazard_mod", "lnhr_noncom_presc", runner_name,
    "build_treatment_metadata_table", "build_treatment_coefficient_table"
  )
  missing_objects <- required_objects[!vapply(
    required_objects,
    exists,
    logical(1),
    envir = state,
    inherits = FALSE
  )]
  if (length(missing_objects) > 0L) {
    stop(
      "Simulation state is missing required objects: ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  function_names <- c(
    runner_name,
    "build_treatment_metadata_table",
    "build_treatment_coefficient_table"
  )
  invalid_functions <- function_names[!vapply(
    function_names,
    function(name) is.function(get(name, envir = state, inherits = FALSE)),
    logical(1)
  )]
  if (length(invalid_functions) > 0L) {
    stop(
      "Simulation state objects expected to be functions: ",
      paste(invalid_functions, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.data.frame(state$all_hazard_mod)) {
    stop("Simulation state object 'all_hazard_mod' must be tabular.",
         call. = FALSE)
  }
  if (!specs$affected_hazard_column %in% names(state$all_hazard_mod)) {
    stop(
      "Treatment affected_hazard_column '",
      specs$affected_hazard_column,
      "' was not found in the simulated hazard table.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validate_treatment_run <- function(run, strategy_family) {
  if (!is.list(run)) {
    stop(strategy_family, " treatment runner must return a list.",
         call. = FALSE)
  }

  required_elements <- c(
    "estimand", "treatment_coef", "eligible", "treatment_cohort",
    "truth_worlds", "truth_treatment_effects"
  )
  if (strategy_family == "episodic") {
    required_elements <- c(required_elements, "triggers", "episode_triggers")
  }

  missing_elements <- setdiff(required_elements, names(run))
  if (length(missing_elements) > 0L) {
    stop(
      strategy_family,
      " treatment result is missing required elements: ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.list(run$estimand)) {
    stop("Treatment result element 'estimand' must be a list.",
         call. = FALSE)
  }
  if (!is.list(run$treatment_coef)) {
    stop("Treatment result element 'treatment_coef' must be a list.",
         call. = FALSE)
  }

  coefficient_elements <- c(
    "treatment_start_log_hr", "treatment_stop_log_hr"
  )
  missing_coefficients <- setdiff(
    coefficient_elements,
    names(run$treatment_coef)
  )
  if (length(missing_coefficients) > 0L) {
    stop(
      "Treatment result is missing coefficient elements: ",
      paste(missing_coefficients, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.list(run$truth_treatment_effects)) {
    stop("Treatment result element 'truth_treatment_effects' must be a list.",
         call. = FALSE)
  }

  invisible(TRUE)
}

.build_ehr_observation_window <- function(state) {
  if (!is.list(state$ehr_tables)) {
    stop("The treatment simulation did not produce final EHR tables.",
         call. = FALSE)
  }

  patient <- state$ehr_tables$patient
  death <- state$ehr_tables$death_linked
  if (!is.data.frame(patient) || !is.data.frame(death)) {
    stop(
      "The final EHR must contain patient and death_linked tables.",
      call. = FALSE
    )
  }

  patient_columns <- c("patient_id", "reg_date", "transfer_date")
  death_columns <- c("patient_id", "death_date_official")
  missing_patient_columns <- setdiff(patient_columns, names(patient))
  missing_death_columns <- setdiff(death_columns, names(death))
  if (length(missing_patient_columns) > 0L ||
      length(missing_death_columns) > 0L) {
    missing_columns <- c(missing_patient_columns, missing_death_columns)
    stop(
      "The final EHR is missing observation-window columns: ",
      paste(unique(missing_columns), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(patient$patient_id) || anyDuplicated(death$patient_id)) {
    stop("Final EHR observation tables must contain one row per patient.",
         call. = FALSE)
  }

  study_end <- state$datestudylast
  if (length(study_end) != 1L || is.na(study_end)) {
    stop("The simulation study-end date is unavailable.", call. = FALSE)
  }

  patient <- data.table::as.data.table(data.table::copy(patient))
  death <- data.table::as.data.table(data.table::copy(death))
  patient <- patient[, patient_columns, with = FALSE]
  data.table::setnames(patient, "reg_date", "ehr_reg_date")
  death <- death[, death_columns, with = FALSE]
  observation <- merge(
    patient,
    death,
    by = "patient_id",
    all.x = TRUE
  )
  observation[["study_end"]] <- as.Date(study_end)
  observation[]
}

.append_cohort_basis_metadata <- function(metadata) {
  data.table::rbindlist(
    list(
      metadata,
      data.frame(
        field = "cohort_basis",
        value = "P7 eligible and observable in P13 at time zero",
        stringsAsFactors = FALSE
      )
    ),
    fill = TRUE
  )
}

.run_sim_for_treatment_truth <- function(pracid,
                                         gp_rel_size,
                                         seed) {
  .load_simulation_packages()
  
  pracid <- as.integer(pracid)
  gp_rel_size <- as.numeric(gp_rel_size)
  seed <- as.integer(seed)
  
  sim_env <- new.env(parent = globalenv())
  sim_env$pracid <- pracid
  sim_env$gp_rel_size <- gp_rel_size
  sim_env$seed <- seed
  set.seed(seed)
  
  truth_script_order <- c(
    "P0_a_Init_functions_comorbidities.R",
    "P0_b_Init_parameters_comorbidities.R",
    "P0_c_Init_function_medications.R",
    "P0_d_Init_parameters_death.R",
    "P0_e_Codes_used.R",
    "P0_f_Init_functions_treatment_effects.R",
    "P1_Simulate_GP.R",
    "P2_Simulate_patients.R",
    "P3_Simulate_frailty.R",
    "P4_Simulate_underlying_clinical_measures.R",
    "P5_a_Simulate_comorbidities_cardiometabolic.R",
    "P5_b_Simulate_comorbidities_otherdis.R",
    "P5_c_Simulate_comorbidities_dates.R",
    "P6_a_Simulate_medication_dates.R",
    "P6_b_Simulate_medications.R",
    "P7_Simulate_death_and_transfer.R"
  )

  ehr_script_order <- c(
    "P8_Update_tables_for_death_and_transfer.R",
    "P9_Simulate_consultations.R",
    "P10_Simulate_clinical_measures.R",
    "P11_Add_clinical_codes.R",
    "P12_Add_prescription_codes.R",
    "P13_Extract_EHRs.R"
  )
  
  pipeline_dir <- system.file("pipeline", package = "EHRSim")
  
  if (!nzchar(pipeline_dir)) {
    stop("Could not locate the packaged simulation pipeline.", call. = FALSE)
  }
  for (script_name in truth_script_order) {
    sys.source(file.path(pipeline_dir, script_name), envir = sim_env)
  }

  snapshot_names <- c(
    "patient", "comorbid_dt", "medication", "all_hazard_mod",
    "drug_groups", "lnhr_noncom_presc"
  )
  p7_snapshot <- lapply(
    snapshot_names,
    function(name) data.table::copy(sim_env[[name]])
  )
  names(p7_snapshot) <- snapshot_names

  for (script_name in ehr_script_order) {
    sys.source(file.path(pipeline_dir, script_name), envir = sim_env)
  }

  sim_env$ehr_tables <- sim_env$tables
  for (name in snapshot_names) {
    sim_env[[name]] <- p7_snapshot[[name]]
  }

  sim_env
}
