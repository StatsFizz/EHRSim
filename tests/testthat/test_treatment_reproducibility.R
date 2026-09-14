test_that("fixed-course treatment truth is reproducible for identical inputs", {
  first <- truth_fixed_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .fixed_course_specs()
  )
  second <- truth_fixed_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .fixed_course_specs()
  )

  expect_equal(first$eligible, second$eligible)
  expect_equal(first$truth_worlds, second$truth_worlds)
  expect_equal(
    first$truth_treatment_effects$contrast_world_population,
    second$truth_treatment_effects$contrast_world_population
  )
})

test_that("episodic treatment truth is reproducible for identical inputs", {
  first <- truth_episodic_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .episodic_specs()
  )
  second <- truth_episodic_course_treatment(
    pracid = 1L,
    gp_rel_size = 1,
    seed = 34637L,
    specs = .episodic_specs()
  )

  expect_equal(first$eligible, second$eligible)
  expect_equal(first$truth_worlds, second$truth_worlds)
  expect_equal(
    first$truth_treatment_effects$contrast_world_population,
    second$truth_treatment_effects$contrast_world_population
  )
})
