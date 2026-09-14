test_that("observation window comes from final P13 follow-up tables", {
  state <- new.env(parent = emptyenv())
  state$ehr_tables <- list(
    patient = data.frame(
      patient_id = c("p1", "p2"),
      reg_date = as.Date(c("2010-01-01", "2012-01-01")),
      transfer_date = as.Date(c(NA, "2020-01-01"))
    ),
    death_linked = data.frame(
      patient_id = "p1",
      death_date_official = as.Date("2021-01-01")
    )
  )
  state$datestudylast <- as.Date("2025-12-31")

  observation <- EHRSim:::.build_ehr_observation_window(state)

  expect_named(
    observation,
    c(
      "patient_id", "ehr_reg_date", "transfer_date",
      "death_date_official", "study_end"
    )
  )
  expect_setequal(observation$patient_id, c("p1", "p2"))
  expect_equal(
    observation$death_date_official[observation$patient_id == "p1"],
    as.Date("2021-01-01")
  )
  expect_true(is.na(
    observation$death_date_official[observation$patient_id == "p2"]
  ))
})

test_that("observability is evaluated at each latent time zero", {
  eligible <- data.table::data.table(
    patient_id = paste0("p", 1:8),
    time_zero = as.Date(c(
      rep("2020-01-01", 4),
      "2026-01-01",
      "2020-01-01",
      "2020-01-01",
      "2020-01-01"
    )),
    marker = seq_len(8)
  )
  observation <- data.table::data.table(
    patient_id = c("p1", "p2", "p3", "p4", "p5", "p6", "p8"),
    ehr_reg_date = as.Date(c(
      "2010-01-01", "2010-01-01", "2010-01-01", "2021-01-01",
      "2010-01-01", "2010-01-01", "2010-01-01"
    )),
    transfer_date = as.Date(c(
      NA, "2019-01-01", NA, NA, NA, "2020-01-01", NA
    )),
    death_date_official = as.Date(c(
      NA, NA, "2019-01-01", NA, NA, NA, "2020-01-01"
    )),
    study_end = as.Date(rep("2025-12-31", 7))
  )

  observed <- .treatment_test_functions$restrict_eligible_to_ehr_observation(
    eligible,
    observation
  )

  expect_setequal(observed$patient_id, c("p1", "p6", "p8"))
  expect_identical(names(observed), names(eligible))
})

test_that("no observation window preserves internal legacy behaviour", {
  eligible <- data.table::data.table(
    patient_id = c("p1", "p2"),
    time_zero = as.Date(c("2020-01-01", "2021-01-01"))
  )

  observed <- .treatment_test_functions$restrict_eligible_to_ehr_observation(
    eligible,
    NULL
  )

  expect_identical(observed, eligible)
})

test_that("invalid observation windows fail clearly", {
  eligible <- data.table::data.table(
    patient_id = "p1",
    time_zero = as.Date("2020-01-01")
  )
  incomplete <- data.frame(patient_id = "p1")

  expect_error(
    .treatment_test_functions$restrict_eligible_to_ehr_observation(
      eligible,
      incomplete
    ),
    "missing required columns"
  )
})
