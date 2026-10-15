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
#' @inheritParams truth_fixed_course_treatment
#' @param specs A named list containing `strategy_type`, `drug_name`,
#'   `population`, `start_trigger_cols`, `time_zero_col`, a positive
#'   `course_duration_days`, `affected_cause`, `affected_hazard_column`, and
#'   positive numeric `horizons`. See the specification sections for meanings
#'   and optional fields.
#'
#' @inheritSection truth_fixed_course_treatment Common specification fields
#' @section Episodic fields:
#' `strategy_type` accepts `"episodic_course"`, `"episodic_triggered"`,
#' `"episodic_course_one_stop"`, or `"episodic_course_restart"`. The trigger
#' columns, duration, delay and `restart_after_stop` flag determine the rule;
#' changing a type label alone does not enable restarting.
#'
#' `start_trigger_cols` contains existing comorbidity date columns. The first
#' qualifying event defines time zero; later available trigger dates can begin
#' further courses. First-MI and first-stroke columns represent first events,
#' rather than every recurrent event. `course_duration_days` is positive and
#' `course_delay_days` is non-negative, defaulting to zero when omitted. Optional
#' `start_trigger_label` labels the coefficient output.
#'
#' `restart_after_stop` is a single logical value, defaulting to `FALSE`. With
#' `FALSE`, the first qualifying patient-level stop applies across later
#' courses. With `TRUE`, each course is matched to the first stop at or after
#' its scheduled start and before its planned end. One stop can therefore
#' affect several overlapping courses. Later triggers can initiate treatment
#' again after an earlier course stopped.
#'
#' Overlapping courses produce binary treatment activity; they do not multiply
#' the treatment effect. Stop fields produce both a no-stopping and a stopping
#' version. Both retain course ends; only the latter applies event-triggered
#' stops. The version names include `episodic_restart` when the restart flag is
#' `TRUE`. Without stop fields, only the no-stopping version is constructed.
#'
#' @inheritSection truth_fixed_course_treatment Eligibility and observation
#' @inheritSection truth_fixed_course_treatment Effect summaries
#'
#' @return A list containing:
#' \describe{
#'   \item{strategy_family}{The treatment-strategy family, here `"episodic"`.}
#'   \item{specs}{The input treatment-effect specification.}
#'   \item{eligible}{The eligible population used for the treatment analysis.}
#'   \item{treatment_cohort}{Follow-up intervals with natural and assigned
#'   exposure flags.}
#'   \item{truth_worlds}{Interval histories under natural, forced-treatment and
#'   forced-no-treatment strategies, with reconstructed hazards and survival.}
#'   \item{truth_treatment_effects}{Treatment-effect truth tables, including
#'   simulation metadata and the treatment coefficients used.}
#'   \item{triggers}{Available trigger dates before eligibility filtering.}
#'   \item{episode_triggers}{Eligible episode triggers with scheduled course
#'   starts, planned ends and stopping-version ends.}
#' }
#' @seealso [truth_fixed_course_treatment()],
#'   [truth_persistent_course_treatment()]. For a walkthrough, use
#'   `vignette("treatment-truth", package = "EHRSim")`.
#' @examples
#' episodic_specs <- list(
#'   strategy_type = "episodic_course_restart",
#'   restart_after_stop = TRUE,
#'   drug_name = "aspirin",
#'   population = "first_vascular_event_aspirin_naive",
#'   start_trigger_label = "first_vascular_event",
#'   start_trigger_cols = c("mi_event_first_date", "stroke_event_first_date",
#'                          "chd_onset_date", "pvd_onset_date",
#'                          "other_athero_onset_date"),
#'   time_zero_col = "first_vascular_event_date",
#'   course_duration_days = 365L,
#'   course_delay_days = 0L,
#'   stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
#'   stop_trigger_cols = c("upper_gi_bleed_event_date", "peptic_ulcer_event_date"),
#'   affected_cause = "non_communicable_death",
#'   affected_hazard_column = "lnhr_all_1",
#'   horizons = c(1, 5, 10)
#' )
#' \dontrun{
#' episodic_truth <- truth_episodic_course_treatment(
#'   pracid = 1L, gp_rel_size = 1, seed = 34637L, specs = episodic_specs
#' )
#' episodic_truth$truth_treatment_effects$contrast_population
#' head(episodic_truth$episode_triggers)
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
