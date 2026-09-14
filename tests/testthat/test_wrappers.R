test_that("simulate_one_gp returns expected structure", {
  res <- simulate_one_gp(write_csv = FALSE)
  
  expect_type(res, "list")
  expect_true(all(c("gp", "tables", "tables_dg", "metadata", "output_dir", "output_paths") %in% names(res)))
  expect_true(all(c("patient", "imd", "death_linked", "consultation", "clinical", "prescription", "supplementary") %in% names(res$tables)))
  expect_true(all(c("patient", "frailty", "clinicalmeasures", "comorbid", "medication", "deathdate", "transfer") %in% names(res$tables_dg)))
})

test_that("simulate_many_gps returns expected structure", {
  gp_specs <- data.frame(
    pracid = c(1L, 2L),
    gp_rel_size = c(10, 8),
    seed = c(34637L, 34638L)
  )
  
  res <- simulate_many_gps(
    gp_specs = gp_specs,
    write_csv = FALSE,
    keep_gp_results = FALSE
  )
  
  expect_type(res, "list")
  expect_true(all(c("gp_specs", "tables", "tables_dg", "metadata", "output_dir", "output_paths", "gp_results") %in% names(res)))
  expect_equal(res$metadata$n_gps, 2)
})

test_that("simulation wrappers reject invalid specifications before running", {
  expect_error(
    simulate_one_gp(pracid = 0L, write_csv = FALSE),
    "pracid must be at least 1"
  )

  empty_specs <- data.frame(
    pracid = integer(),
    gp_rel_size = numeric(),
    seed = integer()
  )
  expect_error(
    simulate_many_gps(empty_specs, write_csv = FALSE),
    "gp_specs must contain at least one practice"
  )

  invalid_specs <- data.frame(
    pracid = 1L,
    gp_rel_size = -1,
    seed = 34637L
  )
  expect_error(
    simulate_many_gps(invalid_specs, write_csv = FALSE),
    "gp_rel_size must be at least 0"
  )
})
