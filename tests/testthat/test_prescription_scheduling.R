.medication_test_functions <- local({
  pipeline_file <- system.file(
    "pipeline",
    "P0_c_Init_function_medications.R",
    package = "EHRSim"
  )
  if (!nzchar(pipeline_file)) {
    stop("Could not locate the packaged medication functions.")
  }

  pipeline_env <- new.env(parent = asNamespace("data.table"))
  sys.source(pipeline_file, envir = pipeline_env)
  pipeline_env
})

.prescription_test_inputs <- function(current_issue,
                                      current_duration,
                                      gap) {
  list(
    dates = list(
      start_date = current_issue + current_duration,
      end_date = rep(as.Date("2030-01-01"), length(current_issue)),
      pdisc = 0,
      maxgap_fn = function(n, adjustment) rep(adjustment, n),
      maxgap_fn_args = list(adjustment = gap)
    ),
    duration = list(
      length_presc = current_duration,
      pchange = 0,
      prescdays_fn = function(n) rep(28, n),
      prescdays_fn_args = list()
    )
  )
}

test_that("early refills overlap supply without reversing issue dates", {
  current_issue <- as.Date(c("2020-01-01", "2020-01-01"))
  current_duration <- c(28, 5)
  inputs <- .prescription_test_inputs(
    current_issue,
    current_duration,
    gap = -7
  )

  result <- .medication_test_functions$draw_next_presc(
    inputs$dates,
    inputs$duration
  )
  next_issue <- as.Date(result[[1]])

  expect_equal(next_issue[1], as.Date("2020-01-22"))
  expect_equal(next_issue[2], current_issue[2])
  expect_true(all(next_issue >= current_issue))
  expect_true(all(next_issue < current_issue + current_duration))
})

test_that("positive refill gaps remain relative to expected supply end", {
  current_issue <- as.Date("2020-01-01")
  current_duration <- 28
  inputs <- .prescription_test_inputs(
    current_issue,
    current_duration,
    gap = 3
  )

  result <- .medication_test_functions$draw_next_presc(
    inputs$dates,
    inputs$duration
  )

  expect_equal(
    as.Date(result[[1]]),
    current_issue + current_duration + 3
  )
})
