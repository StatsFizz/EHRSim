test_that("delayed fixed-course treatment respects start and end boundaries", {
  eligible <- data.table::data.table(
    patient_id = 1L,
    time_zero = as.Date("2020-01-01"),
    first_stop_date = as.Date("2020-01-12"),
    enddate_init = as.Date("2020-02-01")
  )
  specs <- list(
    course_delay_days = 5L,
    course_duration_days = 10L
  )
  eligible <- .treatment_test_functions$add_fixed_course_schedule(
    eligible,
    specs
  )
  hazards <- data.table::data.table(
    patient_id = 1L,
    date = as.Date("2020-01-01"),
    date_end = as.Date("2020-02-01")
  )

  intervals <- .treatment_test_functions$prepare_fixed_course_intervals(
    all_hazard_mod = hazards,
    eligible = eligible
  )

  expect_equal(unique(intervals$course_start_date), as.Date("2020-01-06"))
  expect_equal(unique(intervals$planned_course_end), as.Date("2020-01-16"))
  expect_equal(unique(intervals$actual_course_end), as.Date("2020-01-12"))

  active_with_stopping <- intervals[
    treatment_active_fixed_course_with_stopping == 1L
  ]
  expect_gt(nrow(active_with_stopping), 0L)
  expect_true(all(
    active_with_stopping$date >= active_with_stopping$course_start_date
  ))
  expect_true(all(
    active_with_stopping$date_end <= active_with_stopping$actual_course_end
  ))
  expect_true(all(intervals[
    date < course_start_date | date >= actual_course_end,
    treatment_active_fixed_course_with_stopping
  ] == 0L))

  active_without_stopping <- intervals[
    treatment_active_fixed_course_no_stopping == 1L
  ]
  expect_gt(nrow(active_without_stopping), 0L)
  expect_true(all(
    active_without_stopping$date >= active_without_stopping$course_start_date
  ))
  expect_true(all(
    active_without_stopping$date_end <=
      active_without_stopping$planned_course_end
  ))
  expect_true(all(intervals[
    date < course_start_date | date >= planned_course_end,
    treatment_active_fixed_course_no_stopping
  ] == 0L))
})
