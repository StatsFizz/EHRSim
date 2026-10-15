################################################################################
#
#   Script:       truth_fixed_treatment_strategy.R
#
#   Purpose:      Gather the treatment truth for designated
#                 fixed course strategy.
#
################################################################################
# Create the fixed_course function wrapper as user facing function
#' Simulate truth under a fixed-course treatment strategy
#'
#' Runs the treatment-truth workflow for a single simulated GP practice under a
#' fixed-course treatment strategy and returns the resulting truth objects. The
#' target population is automatically restricted to latent-eligible patients
#' who are observable in the final analyst-visible EHR at time zero.
#'
#' @param pracid One positive integer practice identifier.
#' @param gp_rel_size One positive numeric relative practice size.
#' @param seed One non-negative integer random seed. Use the same practice
#'   inputs, package version and software environment as the corresponding EHR
#'   simulation.
#' @param specs A named list containing `strategy_type`, `drug_name`,
#'   `population`, `start_trigger_cols`, `time_zero_col`, a positive
#'   `course_duration_days`, `affected_cause`, `affected_hazard_column`, and
#'   positive numeric `horizons`. See the specification sections for meanings
#'   and optional fields.
#'
#' @section Common specification fields:
#' * `drug_name`: treatment name or group with configured prescription
#'   coefficients in the simulator. An arbitrary medicine name does not create
#'   a new treatment model.
#' * `population`: a descriptive cohort label. It does not apply a custom
#'   patient filter; eligibility is derived from dates and prior medication use.
#' * `time_zero_col`: name assigned to the derived earliest qualifying date.
#'   This column is created by the engine and need not already exist in the
#'   comorbidity table.
#' * `affected_hazard_column`: existing log-hazard column to modify. The worked
#'   examples use `"lnhr_all_1"`, the non-communicable mortality component.
#'   Prescription coefficients are taken from the simulator's non-communicable
#'   mortality coefficient table; this function does not fit new coefficients.
#' * `affected_cause`: descriptive label for the modified component. Changing
#'   the label alone does not change which hazard is modified.
#' * `horizons`: finite positive follow-up durations in years. These are separate
#'   from treatment-course durations, which are specified in days.
#' * `stop_trigger_cols` and `stop_trigger_labels`: optional stopping-date column
#'   names and corresponding labels. Supply both with matching lengths, or omit
#'   both. Named trigger columns should exist in the comorbidity model.
#'
#' @section Fixed-course fields:
#' `strategy_type` accepts `"fixed_course_once"`, `"fixed_course_delayed"`, or
#' `"multi_trigger_fixed_course"`. These labels describe the intended rule;
#' `start_trigger_cols`, `course_duration_days` and `course_delay_days` determine
#' the schedule. Changing the label alone does not add a delay or new triggers.
#'
#' `start_trigger_cols` contains one or more existing comorbidity date columns,
#' such as `"mi_event_first_date"`. The earliest qualifying date defines time
#' zero. Even with several trigger columns, this function creates one course
#' per patient; later events do not start further courses.
#'
#' `course_duration_days` is one positive duration measured from scheduled
#' initiation. `course_delay_days` is one non-negative delay from time zero;
#' omission means zero. For delayed initiation, set a positive delay explicitly.
#' Eligibility and outcome follow-up remain anchored at time zero, including
#' the untreated delay period. A stopping event before scheduled initiation can
#' prevent the course from becoming active in the stopping version.
#'
#' Optional `start_trigger_label` labels the coefficient output. Supplying stop
#' fields constructs both `fixed_course_no_stopping` and
#' `fixed_course_with_stopping`; omission constructs only the former. Both
#' respect the scheduled course end. The stopping version may end earlier at
#' the first qualifying stop.
#'
#' @section Eligibility and observation:
#' Each call reruns the bundled simulation, retains the P7 hazard histories,
#' and continues through P13 for the EHR observability check. Eligible patients
#' have a qualifying trigger, no selected-group prescription before time zero,
#' and observable EHR follow-up at time zero according to registration, transfer,
#' linked death and study-end dates. Same-day first treatment and observation
#' endpoint dates are allowed. A recorded diagnosis code is not required.
#'
#' The function constructs the default eligible cohort; it does not read an
#' existing EHR object or infer a separately filtered analysis cohort. Transfer
#' after time zero ends EHR observation and does not automatically stop the
#' latent assigned treatment strategy. An empty eligible cohort causes an error.
#'
#' @section Effect summaries:
#' Start with `truth_treatment_effects$contrast_population`. Each row contrasts
#' assigned treatment with assigned no treatment for one `truth_version` and
#' `horizon_years`. `mean_survival_diff` and `mean_risk_diff` are probability
#' differences; `mean_rmst_days_diff` is the difference in restricted mean
#' survival time in days. Positive RMST differences mean longer expected
#' survival under the assigned treatment rule. Outcomes combine all three
#' mortality components, although the treatment modifies one component.
#'
#' `n_patients` counts patients represented in the contrast table. `n_complete`
#' counts those with retained hazard follow-up through the requested horizon;
#' only these patients contribute to the full-horizon mean. Different horizons
#' can therefore use different subsets. Means can be `NaN` when no patients
#' contribute; that is not evidence of a zero effect.
#'
#' Other shared tables include `contrast_patient`, `horizon_patient`,
#' `horizon_strategy`, `horizon_world`, `survival_intervals`,
#' `survival_curve_patient`, `survival_curve_mean`, `metadata` and `coefficients`.
#' Use `metadata` to inspect the estimand and `coefficients` to inspect the
#' configured hazard effect. Shared natural and no-treatment references are
#' returned once in cleaned interval outputs; the contrast tables retain all
#' requested stopping versions.
#'
#' Treatment truth is conditional on the retained patient and hazard histories.
#' Changing treatment does not regenerate disease dates, other trajectories or
#' trigger events under every intervention. These calculations do not by
#' themselves establish external calibration or estimator performance.
#'
#' @return A list containing:
#' \describe{
#'   \item{strategy_family}{The treatment-strategy family, here `"fixed"`.}
#'   \item{specs}{The input treatment-effect specification.}
#'   \item{eligible}{Eligible patients, time zero, and the planned and stopping
#'   course dates. `actual_course_end` describes the stopping version.}
#'   \item{treatment_cohort}{Follow-up intervals with natural and assigned
#'   exposure flags.}
#'   \item{truth_worlds}{Interval histories under natural, forced-treatment and
#'   forced-no-treatment strategies, with reconstructed hazards and survival.}
#'   \item{truth_treatment_effects}{Treatment-effect truth tables, including
#'   simulation metadata and the treatment coefficients used.}
#' }
#' @seealso [truth_persistent_course_treatment()],
#'   [truth_episodic_course_treatment()]. For a walkthrough, use
#'   `vignette("treatment-truth", package = "EHRSim")`.
#' @examples
#' fixed_specs <- list(
#'   strategy_type = "fixed_course_once",
#'   drug_name = "aspirin",
#'   population = "first_mi_aspirin_naive",
#'   start_trigger_label = "first_mi",
#'   start_trigger_cols = "mi_event_first_date",
#'   time_zero_col = "first_mi_date",
#'   course_duration_days = 365L,
#'   course_delay_days = 0L,
#'   affected_cause = "non_communicable_death",
#'   affected_hazard_column = "lnhr_all_1",
#'   horizons = c(1, 5, 10)
#' )
#' \dontrun{
#' # Runs the complete simulation for this practice.
#' fixed_truth <- truth_fixed_course_treatment(
#'   pracid = 1L, gp_rel_size = 1, seed = 34637L, specs = fixed_specs
#' )
#' fixed_truth$truth_treatment_effects$contrast_population
#' }
#' @export
truth_fixed_course_treatment <- function(pracid, gp_rel_size, seed, specs){
  .validate_treatment_inputs(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed,
    specs = specs,
    strategy_family = "fixed"
  )
  
  state <- .run_sim_for_treatment_truth(
    pracid = pracid,
    gp_rel_size = gp_rel_size,
    seed = seed
  )
  
  .validate_treatment_state(
    state,
    "run_fixed_course_treatment_effect",
    specs
  )

  ehr_observation_window <- .build_ehr_observation_window(state)
  
  run <- state$run_fixed_course_treatment_effect(patient = state$patient,
                                         comorbid_dt = state$comorbid_dt,
                                         medication = state$medication,
                                         drug_groups = state$drug_groups,
                                         all_hazard_mod = state$all_hazard_mod,
                                         specs = specs,
                                         prescription_log_hr_coefficients = state$lnhr_noncom_presc,
                                         ehr_observation_window = ehr_observation_window)
  .validate_treatment_run(run, "fixed")

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
    strategy_family = "fixed",
    specs = specs,
    eligible = run$eligible,
    treatment_cohort = run$treatment_cohort,
    truth_worlds = run$truth_worlds,
    truth_treatment_effects = truth_treatment_effects
  )
  result
}
