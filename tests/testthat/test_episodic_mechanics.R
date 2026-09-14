.treatment_pipeline_functions <- local({
  pipeline_file <- system.file(
    "pipeline",
    "P0_f_Init_functions_treatment_effects.R",
    package = "EHRSim"
  )
  if (!nzchar(pipeline_file)) {
    stop("Could not locate the packaged treatment-effect functions.")
  }

  pipeline_env <- new.env(parent = asNamespace("data.table"))
  sys.source(pipeline_file, envir = pipeline_env)
  pipeline_env
})

test_that("one stop event ends every overlapping episodic course", {
  episodes <- data.table::data.table(
    patient_id = c(1L, 1L),
    episode_id = c(1L, 2L),
    course_start_date = as.Date(c("2020-01-01", "2020-01-11")),
    planned_course_end = as.Date(c("2020-01-31", "2020-02-10"))
  )
  comorbid <- data.table::data.table(
    patient_id = 1L,
    bleed_date = as.Date("2020-01-21")
  )

  stops <- .treatment_pipeline_functions$build_episode_stop_table(
    episode_triggers = episodes,
    comorbid_dt = comorbid,
    specs = list(stop_trigger_cols = "bleed_date")
  )

  expect_equal(sort(stops$episode_id), c(1L, 2L))
  expect_equal(
    stops$episode_stop_date[order(stops$episode_id)],
    as.Date(c("2020-01-21", "2020-01-21"))
  )
  expect_equal(
    as.character(unique(stops$episode_stop_event)),
    "bleed_date"
  )
})

test_that("episodic boundaries split one interval in a single ordered pass", {
  treatment_cohort <- data.table::data.table(
    patient_id = 1L,
    date = as.Date("2020-01-01"),
    date_end = as.Date("2020-02-10")
  )
  episodes <- data.table::data.table(
    patient_id = 1L,
    time_zero = as.Date("2020-01-01"),
    enddate_init = as.Date("2020-02-11"),
    course_start_date = as.Date("2020-01-11"),
    planned_course_end = as.Date("2020-01-31"),
    actual_course_end = as.Date("2020-01-21")
  )

  split <- .treatment_pipeline_functions$split_intervals_at_episode_boundaries(
    treatment_cohort = treatment_cohort,
    episode_triggers = episodes
  )
  split <- as.data.frame(split)
  split <- split[order(split$date), , drop = FALSE]

  expect_equal(
    split$date,
    as.Date(c("2020-01-01", "2020-01-11", "2020-01-21", "2020-01-31"))
  )
  expect_equal(
    split$date_end,
    as.Date(c("2020-01-11", "2020-01-21", "2020-01-31", "2020-02-10"))
  )
  expect_true(all(split$date < split$date_end))
})

test_that("shared cohort construction reports empty cohorts clearly", {
  no_triggers <- data.table::data.table(
    patient_id = c(1L, 2L),
    trigger_date = as.Date(c(NA, NA))
  )

  expect_error(
    .treatment_pipeline_functions$build_new_user_trigger_cohort(
      comorbid_dt = no_triggers,
      patient = data.table::data.table(),
      medication = data.table::data.table(),
      drug_group = "aspirin",
      population = "test_population",
      start_trigger_cols = "trigger_date",
      time_zero_col = "time_zero_date"
    ),
    "No eligible patients found.*no observed dates"
  )

  trigger_outside_follow_up <- data.table::data.table(
    patient_id = 1L,
    trigger_date = as.Date("2020-01-01")
  )
  patient <- data.table::data.table(
    patient_id = 1L,
    dob = as.Date("1980-01-01"),
    sex = "F",
    reg_date = as.Date("2021-01-01"),
    enddate_init = as.Date("2022-01-01")
  )
  medication <- data.table::data.table(
    patient_id = 2L,
    drug = "aspirin",
    date = as.Date("2020-01-01")
  )

  expect_error(
    .treatment_pipeline_functions$build_new_user_trigger_cohort(
      comorbid_dt = trigger_outside_follow_up,
      patient = patient,
      medication = medication,
      drug_group = "aspirin",
      population = "test_population",
      start_trigger_cols = "trigger_date",
      time_zero_col = "time_zero_date"
    ),
    "No eligible patients found.*eligibility criteria"
  )
})
