test_that("simulate_one_gp returns expected structure", {
  res <- simulate_one_gp(write_csv = FALSE)
  
  expect_type(res, "list")
  expect_true(all(c("gp", "tables", "tables_dg", "metadata", "output_dir", "output_paths") %in% names(res)))
  expect_true(all(c("patient", "imd", "death_linked", "consultation", "clinical", "prescription", "supplementary") %in% names(res$tables)))
  expect_true(all(c("patient", "frailty", "clinicalmeasures", "comorbid", "medication", "deathdate", "transfer") %in% names(res$tables_dg)))

  clinical <- data.table::as.data.table(res$tables$clinical)
  bmi_codes <- c(1L, 1626L, 2298L, 2300L, 2310L, 2299L, 2301L,
                 1625L, 2302L, 2303L, 1627L, 2304L, 2305L,
                 1628L, 2306L, 2307L, 1629L, 2308L, 2309L)
  expect_true(any(clinical$entity == 36L))
  expect_true(all(clinical[entity == 36L, clinical_code] %in% bmi_codes))
  expect_false(any(clinical[entity == 36L, clinical_code] == 1624L))
  expect_true(all(clinical[entity == 1L & suppl_info_id != "0", clinical_code] == 2L))
  expect_true(all(clinical[entity == 2L & suppl_info_id != "0", clinical_code] == 3L))
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
