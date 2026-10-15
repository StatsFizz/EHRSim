################################################################################
#
#   Script:       truth_persistent_treatment_strategy.R
#
#   Purpose:      Gather the treatment truth for designated
#                 persistent strategy.
#
################################################################################

###################################
# Create the persistent function wrapper as user facing function
#' Simulate truth under a persistent treatment strategy
#'
#' Runs the treatment-truth workflow for a single simulated GP practice under a
#' persistent treatment strategy and returns the resulting truth objects. The
#' target population is automatically restricted to latent-eligible patients
#' who are observable in the final analyst-visible EHR at time zero.
#'
#' @inheritParams truth_fixed_course_treatment
#' @param specs A named list containing `drug_name`, `population`,
#'   `start_trigger_label`, `strategy_context_label`, `start_trigger_col`,
#'   `time_zero_col`, `affected_cause`, `affected_hazard_column`, and positive
#'   numeric `horizons`. See the specification sections for meanings and
#'   optional fields.
#'
#' @inheritSection truth_fixed_course_treatment Common specification fields
#' @section Persistent fields:
#' `start_trigger_col` is one existing comorbidity date column, for example
#' `"mi_event_first_date"`. It defines the first qualifying date and initiation
#' time. Use the singular field name for this wrapper.
#'
#' `start_trigger_label` and `strategy_context_label` are required descriptive
#' labels. The persistent rule has no fixed course duration or initiation-delay
#' field. Assigned treatment begins at time zero and continues through the
#' retained hazard follow-up. With stopping fields, the engine constructs
#' `persistent_no_stopping` and `persistent_with_stopping`; the latter stops at
#' the first qualifying event. There is no restarting after that stop in this
#' family. Without stop fields, only the no-stopping version is constructed.
#'
#' @inheritSection truth_fixed_course_treatment Eligibility and observation
#' @inheritSection truth_fixed_course_treatment Effect summaries
#'
#' @return A list containing:
#' \describe{
#'   \item{strategy_family}{The treatment-strategy family, here `"persistent"`.}
#'   \item{specs}{The input treatment-effect specification.}
#'   \item{eligible}{The eligible population used for the treatment analysis.}
#'   \item{treatment_cohort}{Follow-up intervals with natural and assigned
#'   exposure flags.}
#'   \item{truth_worlds}{Interval histories under natural, forced-treatment and
#'   forced-no-treatment strategies, with reconstructed hazards and survival.}
#'   \item{truth_treatment_effects}{Treatment-effect truth tables, including
#'   simulation metadata and the treatment coefficients used.}
#' }
#' @seealso [truth_fixed_course_treatment()],
#'   [truth_episodic_course_treatment()]. For a walkthrough, use
#'   `vignette("treatment-truth", package = "EHRSim")`.
#' @examples
#' persistent_specs <- list(
#'   drug_name = "aspirin",
#'   population = "first_mi_aspirin_naive",
#'   start_trigger_label = "first_mi",
#'   strategy_context_label = "after_first_mi",
#'   start_trigger_col = "mi_event_first_date",
#'   time_zero_col = "first_mi_date",
#'   stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
#'   stop_trigger_cols = c("upper_gi_bleed_event_date", "peptic_ulcer_event_date"),
#'   affected_cause = "non_communicable_death",
#'   affected_hazard_column = "lnhr_all_1",
#'   horizons = c(1, 5, 10)
#' )
#' \dontrun{
#' persistent_truth <- truth_persistent_course_treatment(
#'   pracid = 1L, gp_rel_size = 1, seed = 34637L, specs = persistent_specs
#' )
#' persistent_truth$truth_treatment_effects$contrast_population
#' }
#' @export
truth_persistent_course_treatment <- function(pracid, gp_rel_size, seed, specs){
  .validate_treatment_inputs(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed,
    specs = specs,
    strategy_family = "persistent"
  )
  
  state <- .run_sim_for_treatment_truth(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed
  )
  
  .validate_treatment_state(
    state,
    "run_persistent_treatment_effect",
    specs
  )

  ehr_observation_window <- .build_ehr_observation_window(state)
  
  run <- state$run_persistent_treatment_effect(patient = state$patient,
                                         comorbid_dt = state$comorbid_dt,
                                         medication = state$medication,
                                         drug_groups = state$drug_groups,
                                         all_hazard_mod = state$all_hazard_mod,
                                         specs = specs,
                                         prescription_log_hr_coefficients = state$lnhr_noncom_presc,
                                         ehr_observation_window = ehr_observation_window)
  .validate_treatment_run(run, "persistent")
  
  truth_treatment_effects <- run$truth_treatment_effects
  truth_treatment_effects$metadata <-
    .append_cohort_basis_metadata(
      state$build_treatment_metadata_table(run$estimand)
    )
  truth_treatment_effects$coefficients <-
    state$build_treatment_coefficient_table(
      estimand = run$estimand,
      specs = specs,
      treatment_start_log_hr = run$treatment_coef$treatment_start_log_hr,
      treatment_stop_log_hr = run$treatment_coef$treatment_stop_log_hr
    )
  
  result <- list(
    strategy_family = "persistent",
    specs = specs,
    eligible = run$eligible,
    treatment_cohort = run$treatment_cohort,
    truth_worlds = run$truth_worlds,
    truth_treatment_effects = truth_treatment_effects
  )
  result
}
