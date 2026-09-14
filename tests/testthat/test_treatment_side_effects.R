test_that("treatment wrappers do not alter subsequent base simulations", {
  base_arguments <- list(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    write_csv = FALSE
  )
  before <- do.call(simulate_one_gp, base_arguments)

  invisible(truth_persistent_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .persistent_specs()
  ))

  after <- do.call(simulate_one_gp, base_arguments)

  expect_equal(before$tables, after$tables)
  expect_equal(before$tables_dg, after$tables_dg)
})
