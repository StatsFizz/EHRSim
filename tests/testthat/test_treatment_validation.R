test_that("treatment wrappers reject invalid simulation arguments", {
  expect_error(
    truth_persistent_course_treatment(
      pracid = 0L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = .persistent_specs()
    ),
    "pracid must be at least 1"
  )

  expect_error(
    truth_persistent_course_treatment(
      pracid = 1L,
      gp_rel_size = 0,
      seed = 34637L,
      specs = .persistent_specs()
    ),
    "gp_rel_size must be greater than 0"
  )

  expect_error(
    truth_persistent_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = -1L,
      specs = .persistent_specs()
    ),
    "seed must be at least 0"
  )
})

test_that("treatment wrappers reject malformed specifications", {
  expect_error(
    truth_persistent_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = unname(.persistent_specs())
    ),
    "name for every element"
  )

  missing_drug <- .fixed_course_specs()
  missing_drug$drug_name <- NULL
  expect_error(
    truth_fixed_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = missing_drug
    ),
    "missing required elements: drug_name"
  )

  zero_duration <- .fixed_course_specs()
  zero_duration$course_duration_days <- 0L
  expect_error(
    truth_fixed_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = zero_duration
    ),
    "course_duration_days must be greater than 0"
  )

  invalid_horizons <- .persistent_specs()
  invalid_horizons$horizons <- c(1, 0, Inf)
  expect_error(
    truth_persistent_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = invalid_horizons
    ),
    "horizons must contain finite positive numeric values"
  )

  mismatched_stops <- .persistent_specs()
  mismatched_stops$stop_trigger_cols <- "upper_gi_bleed_event_date"
  expect_error(
    truth_persistent_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = mismatched_stops
    ),
    "must have the same length"
  )
})

test_that("treatment state validation rejects an unknown hazard column", {
  state <- new.env(parent = emptyenv())
  state$patient <- data.frame()
  state$comorbid_dt <- data.frame()
  state$medication <- data.frame()
  state$drug_groups <- list()
  state$all_hazard_mod <- data.frame(lnhr_all_1 = numeric())
  state$lnhr_noncom_presc <- list()
  state$run_persistent_treatment_effect <- function(...) list()
  state$build_treatment_metadata_table <- function(...) data.frame()
  state$build_treatment_coefficient_table <- function(...) data.frame()

  specs <- .persistent_specs()
  specs$affected_hazard_column <- "unknown_hazard"

  expect_error(
    EHRSim:::.validate_treatment_state(
      state,
      "run_persistent_treatment_effect",
      specs
    ),
    "affected_hazard_column 'unknown_hazard' was not found"
  )
})

test_that("strategy-specific wrappers reject specifications from another family", {
  expect_error(
    truth_fixed_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = .episodic_specs()
    ),
    "not valid for the fixed treatment wrapper"
  )

  expect_error(
    truth_episodic_course_treatment(
      pracid = 1L,
      gp_rel_size = 1,
      seed = 34637L,
      specs = .fixed_course_specs()
    ),
    "not valid for the episodic treatment wrapper"
  )
})
