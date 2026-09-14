################################################################################
#
#   Script:       truth_episodic_treatment_strategy.R
#
#   Purpose:      Gather the treatment truth for designated
#                 episodic strategy.
#
################################################################################
# Create the episodic function wrapper as user facing function
#' Simulate truth under an episodic treatment strategy
#'
#' Runs the treatment-truth workflow for a single simulated GP practice under
#' an episodic treatment strategy and returns the resulting truth objects. The
#' target population is automatically restricted to latent-eligible patients
#' who are observable in the final analyst-visible EHR at time zero.
#'
#' @param pracid Integer practice identifier.
#' @param gp_rel_size Numeric relative size of the GP practice.
#' @param seed Integer random seed used for reproducibility.
#' @param specs A named list containing `strategy_type`, `drug_name`,
#'   `population`, `start_trigger_cols`, `time_zero_col`, a positive
#'   `course_duration_days`, `affected_cause`, `affected_hazard_column`, and
#'   positive numeric `horizons`. Optional stopping labels and columns must be
#'   supplied together.
#'
#' @return A list containing:
#' \describe{
#'   \item{strategy_family}{The treatment-strategy family, here `"episodic"`.}
#'   \item{specs}{The input treatment-effect specification.}
#'   \item{eligible}{The eligible population used for the treatment analysis.}
#'   \item{treatment_cohort}{The post-eligibility treatment cohort used in the analysis.}
#'   \item{truth_worlds}{The simulated intervention worlds generated for the strategy.}
#'   \item{truth_treatment_effects}{Treatment-effect truth tables, including
#'   simulation metadata and the treatment coefficients used.}
#'   \item{triggers}{The trigger information used to define episodic treatment starts.}
#'   \item{episode_triggers}{Episode-level trigger information for the episodic strategy.}
#' }
#' @export
truth_episodic_course_treatment <- function(pracid, gp_rel_size, seed, specs){
  .validate_treatment_inputs(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed,
    specs = specs,
    strategy_family = "episodic"
  )
  
  state <- .run_sim_for_treatment_truth(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed
  )
  
  .validate_treatment_state(
    state,
    "run_episodic_treatment_effect",
    specs
  )

  ehr_observation_window <- .build_ehr_observation_window(state)
  
  run <- state$run_episodic_treatment_effect(patient = state$patient,
                                           comorbid_dt = state$comorbid_dt,
                                           medication = state$medication,
                                           drug_groups = state$drug_groups,
                                           all_hazard_mod = state$all_hazard_mod,
                                           specs = specs,
                                           prescription_log_hr_coefficients = state$lnhr_noncom_presc,
                                           ehr_observation_window = ehr_observation_window)
  .validate_treatment_run(run, "episodic")
  
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
    strategy_family = "episodic",
    specs = specs,
    eligible = run$eligible,
    treatment_cohort = run$treatment_cohort,
    truth_worlds = run$truth_worlds,
    truth_treatment_effects = truth_treatment_effects,
    triggers = run$triggers,
    episode_triggers = run$episode_triggers
  )
  result
}
