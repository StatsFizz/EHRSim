.expect_truth_result <- function(res,
                                 strategy_family,
                                 extra_names = character(),
                                 extra_effect_names = character()) {
  expect_type(res, "list")
  expect_equal(res$strategy_family, strategy_family)

  expected_names <- c(
    "strategy_family",
    "specs",
    "eligible",
    "treatment_cohort",
    "truth_worlds",
    "truth_treatment_effects",
    extra_names
  )
  expect_true(all(expected_names %in% names(res)))

  expect_type(res$truth_treatment_effects, "list")
  expected_effect_names <- c(
    "horizon_patient",
    "horizon_strategy",
    "horizon_world",
    "contrast_patient",
    "contrast_population",
    "survival_intervals",
    "survival_curve_patient",
    "survival_curve_mean",
    "metadata",
    "coefficients",
    extra_effect_names
  )
  expect_true(all(expected_effect_names %in% names(res$truth_treatment_effects)))
}

.expect_cohort_basis <- function(res) {
  metadata <- as.data.frame(res$truth_treatment_effects$metadata)
  cohort_basis <- metadata$value[metadata$field == "cohort_basis"]

  expect_length(cohort_basis, 1L)
  expect_equal(
    cohort_basis,
    "P7 eligible and observable in P13 at time zero"
  )
  expect_false("cohort" %in% names(res))
}

.expect_clean_truth_versions <- function(res) {
  collect_character_values <- function(value) {
    if (is.character(value)) {
      return(value)
    }
    if (is.list(value)) {
      return(unlist(lapply(value, collect_character_values), use.names = FALSE))
    }
    character()
  }

  all_character_values <- collect_character_values(res)
  expect_false(any(startsWith(all_character_values, "V1_"), na.rm = TRUE))

  reference_strategies <- c(
    "natural",
    paste0("forced_no_", res$specs$drug_name)
  )
  reference_worlds <- res$truth_worlds[
    res$truth_worlds$strategy %in% reference_strategies,
  ]
  key_columns <- c("patient_id", "strategy", "follow_start", "follow_end")
  expect_false(any(duplicated(reference_worlds[, key_columns, with = FALSE])))
}

.expect_truth_mechanics <- function(res) {
  worlds <- as.data.frame(res$truth_worlds)
  required_world_cols <- c(
    "patient_id", "truth_version", "strategy", "follow_start",
    "follow_end", "interval_length", "treatment_active"
  )

  expect_gt(nrow(worlds), 0L)
  expect_true(all(required_world_cols %in% names(worlds)))
  expect_false(anyNA(worlds[required_world_cols]))
  expect_true(all(worlds$follow_start < worlds$follow_end))
  expect_true(all(worlds$interval_length > 0))
  expect_true(all(worlds$treatment_active %in% c(0L, 1L)))

  horizon <- as.data.frame(res$truth_treatment_effects$horizon_patient)
  complete <- horizon[
    horizon$complete_horizon &
      stats::complete.cases(horizon[c("survival", "risk", "rmst")]),
    ,
    drop = FALSE
  ]

  expect_gt(nrow(complete), 0L)
  expect_true(all(complete$survival >= 0 & complete$survival <= 1))
  expect_true(all(complete$risk >= 0 & complete$risk <= 1))
  expect_equal(complete$risk, 1 - complete$survival, tolerance = 1e-10)
  expect_true(all(complete$rmst >= 0))
  expect_true(all(complete$rmst <= complete$horizon_years + 1e-10))
}

.expect_beneficial_active_treatment <- function(res) {
  effects <- as.data.frame(
    res$truth_treatment_effects$contrast_population
  )
  effect_cols <- c(
    "mean_survival_diff", "mean_risk_diff", "mean_rmst_diff"
  )
  effects <- effects[
    stats::complete.cases(effects[effect_cols]),
    ,
    drop = FALSE
  ]

  expect_gt(nrow(effects), 0L)
  expect_true(all(effects$mean_survival_diff >= -1e-10))
  expect_true(all(effects$mean_risk_diff <= 1e-10))
  expect_true(all(effects$mean_rmst_diff >= -1e-10))
}

.expect_stopping_not_longer_than_no_stopping <- function(res) {
  worlds <- as.data.frame(res$truth_worlds)
  forced_strategy <- paste0("forced_", res$specs$drug_name)
  forced <- worlds[worlds$strategy == forced_strategy, , drop = FALSE]
  forced$active_time <- forced$interval_length * forced$treatment_active

  durations <- stats::aggregate(
    active_time ~ patient_id + truth_version,
    data = forced,
    FUN = sum
  )
  no_stopping <- durations[
    grepl("no_stopping", durations$truth_version, fixed = TRUE),
    c("patient_id", "active_time")
  ]
  with_stopping <- durations[
    grepl("with_stopping", durations$truth_version, fixed = TRUE),
    c("patient_id", "active_time")
  ]
  names(no_stopping)[2] <- "active_time_no_stopping"
  names(with_stopping)[2] <- "active_time_with_stopping"
  paired <- merge(no_stopping, with_stopping, by = "patient_id")

  expect_gt(nrow(paired), 0L)
  expect_true(all(
    paired$active_time_with_stopping <=
      paired$active_time_no_stopping + 1e-10
  ))
}

test_that("truth_persistent_course_treatment returns expected structure", {
  res <- truth_persistent_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637,
    specs = .persistent_specs()
  )

  .expect_truth_result(
    res,
    "persistent",
    extra_effect_names = "contrast_world_population"
  )
  .expect_cohort_basis(res)
  .expect_clean_truth_versions(res)
  .expect_truth_mechanics(res)
  .expect_beneficial_active_treatment(res)
  .expect_stopping_not_longer_than_no_stopping(res)
})

test_that("truth_fixed_course_treatment returns expected structure", {
  res <- truth_fixed_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637,
    specs = .fixed_course_specs()
  )

  .expect_truth_result(
    res,
    "fixed",
    extra_effect_names = c(
      "contrast_world_patient", "contrast_world_population"
    )
  )
  .expect_cohort_basis(res)
  .expect_clean_truth_versions(res)
  .expect_truth_mechanics(res)
  .expect_beneficial_active_treatment(res)
  .expect_stopping_not_longer_than_no_stopping(res)
})

test_that("truth_episodic_course_treatment returns expected structure", {
  res <- truth_episodic_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637,
    specs = .episodic_specs()
  )

  .expect_truth_result(
    res,
    "episodic",
    extra_names = c("triggers", "episode_triggers"),
    extra_effect_names = c(
      "contrast_world_patient", "contrast_world_population"
    )
  )
  .expect_cohort_basis(res)
  .expect_clean_truth_versions(res)
  .expect_truth_mechanics(res)
  .expect_beneficial_active_treatment(res)
  .expect_stopping_not_longer_than_no_stopping(res)
})

test_that("persistent treatment truth is reproducible for identical inputs", {
  first <- truth_persistent_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .persistent_specs()
  )
  second <- truth_persistent_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .persistent_specs()
  )

  expect_equal(first$eligible, second$eligible)
  expect_equal(first$truth_worlds, second$truth_worlds)
  expect_equal(
    first$truth_treatment_effects$contrast_world_population,
    second$truth_treatment_effects$contrast_world_population
  )
})
