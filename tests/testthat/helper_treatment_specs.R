.persistent_specs <- function() {
  list(
    drug_name = "aspirin",
    population = "first_mi_aspirin_naive",
    start_trigger_label = "first_mi",
    strategy_context_label = "after_first_mi",
    start_trigger_col = "mi_event_first_date",
    time_zero_col = "first_mi_date",
    stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
    stop_trigger_cols = c(
      "upper_gi_bleed_event_date",
      "peptic_ulcer_event_date"
    ),
    affected_cause = "non_communicable_death",
    affected_hazard_column = "lnhr_all_1",
    horizons = c(1, 5, 10)
  )
}

.fixed_course_specs <- function() {
  list(
    strategy_type = "fixed_course_once",
    drug_name = "aspirin",
    population = "first_mi_aspirin_naive",
    start_trigger_labels = "first_mi",
    start_trigger_cols = "mi_event_first_date",
    start_trigger_rule = "first",
    strategy_context_label = "after_first_mi",
    time_zero_col = "first_mi_date",
    course_duration_days = 365L,
    course_delay_days = 0L,
    course_duration_label = "365_days",
    stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
    stop_trigger_cols = c(
      "upper_gi_bleed_event_date",
      "peptic_ulcer_event_date"
    ),
    affected_cause = "non_communicable_death",
    affected_hazard_column = "lnhr_all_1",
    horizons = c(1, 5, 10)
  )
}

.episodic_specs <- function() {
  list(
    strategy_type = "episodic_course_restart",
    restart_after_stop = TRUE,
    drug_name = "aspirin",
    population = "first_vascular_event_aspirin_naive",
    start_trigger_label = "first_vascular_event",
    start_trigger_labels = "first_vascular_event",
    strategy_context_label = "after_first_vascular_event",
    start_trigger_cols = c(
      "mi_event_first_date",
      "stroke_event_first_date",
      "chd_onset_date",
      "pvd_onset_date",
      "other_athero_onset_date"
    ),
    start_trigger_rule = "each_trigger_after_time_zero",
    time_zero_col = "first_vascular_event_date",
    course_duration_days = 365L,
    course_delay_days = 0L,
    course_duration_label = "365_days",
    stop_trigger_labels = c("upper_gi_bleed", "peptic_ulcer"),
    stop_trigger_cols = c(
      "upper_gi_bleed_event_date",
      "peptic_ulcer_event_date"
    ),
    affected_cause = "non_communicable_death",
    affected_hazard_column = "lnhr_all_1",
    horizons = c(1, 5, 10)
  )
}
