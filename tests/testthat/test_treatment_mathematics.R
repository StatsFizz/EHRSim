test_that("natural exposure is removed once and strategy exposure is added once", {
  treatment_log_hr <- log(0.8)
  untreated_log_hazard <- log(0.1)
  toy <- data.table::data.table(
    patient_id = 1L,
    follow_start = c(0, 1),
    follow_end = c(1, 2),
    interval_length = c(1, 1),
    lnhr_all_1 = c(
      untreated_log_hazard + treatment_log_hr,
      untreated_log_hazard
    ),
    lnhr_all_2 = log(0.02),
    lnhr_all_3 = log(0.03),
    treatment_active_natural = c(1L, 0L)
  )
  toy[, treatment_log_hr_natural := data.table::fifelse(
    treatment_active_natural == 1L,
    treatment_log_hr,
    0
  )]
  toy[, lnhr_affected_no_treatment :=
        lnhr_all_1 - treatment_log_hr_natural]

  expect_equal(
    toy$lnhr_affected_no_treatment,
    rep(untreated_log_hazard, 2)
  )

  natural <- .treatment_test_functions$build_strategy_world(
    base_dt = toy,
    truth_version = "toy",
    strategy_name = "natural",
    treatment_active = toy$treatment_active_natural,
    treatment_start_log_hr = treatment_log_hr
  )
  forced <- .treatment_test_functions$build_strategy_world(
    base_dt = toy,
    truth_version = "toy",
    strategy_name = "forced_treatment",
    treatment_active = 1L,
    treatment_start_log_hr = treatment_log_hr
  )
  never <- .treatment_test_functions$build_strategy_world(
    base_dt = toy,
    truth_version = "toy",
    strategy_name = "never_treatment",
    treatment_active = 0L,
    treatment_start_log_hr = treatment_log_hr
  )

  expect_equal(natural$lnhr_all_1, toy$lnhr_all_1)
  expect_equal(
    forced$lnhr_all_1,
    toy$lnhr_affected_no_treatment + treatment_log_hr
  )
  expect_equal(
    forced$lnhr_all_1 - toy$lnhr_affected_no_treatment,
    rep(treatment_log_hr, 2)
  )
  expect_equal(never$lnhr_all_1, toy$lnhr_affected_no_treatment)
})

test_that("all-cause hazard is the sum of cause-specific hazards", {
  toy <- data.table::data.table(
    truth_version = "toy",
    patient_id = 1L,
    strategy = "natural",
    follow_start = 0,
    follow_end = 1,
    interval_length = 1,
    lnhr_all_1 = log(0.1),
    lnhr_all_2 = log(0.2),
    lnhr_all_3 = log(0.3)
  )

  result <- .treatment_test_functions$recalculate_strategy_hazards(toy)

  expect_equal(result$h_1, 0.1)
  expect_equal(result$h_2, 0.2)
  expect_equal(result$h_3, 0.3)
  expect_equal(result$h_all_cause, 0.1 + 0.2 + 0.3)
})

test_that("RMST under a constant hazard matches the analytical integral", {
  hazard <- 0.2
  horizon <- 1.5
  toy <- data.table::data.table(
    truth_version = "toy",
    patient_id = 1L,
    strategy = "natural",
    follow_start = 0,
    follow_end = 2,
    interval_length = 2,
    lnhr_all_1 = log(0.1),
    lnhr_all_2 = log(0.05),
    lnhr_all_3 = log(0.05)
  )
  toy <- .treatment_test_functions$recalculate_strategy_hazards(toy)

  result <- .treatment_test_functions$calc_horizon_summary(toy, horizon)
  expected_rmst <- (1 - exp(-hazard * horizon)) / hazard

  expect_true(result$complete_horizon)
  expect_equal(result$survival, exp(-hazard * horizon), tolerance = 1e-12)
  expect_equal(result$rmst, expected_rmst, tolerance = 1e-12)
})
