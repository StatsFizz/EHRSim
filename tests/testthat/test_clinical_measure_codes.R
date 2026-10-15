test_that("anthropometric code definitions are distinct", {
  code_file <- system.file(
    "pipeline",
    "P0_e_Codes_used.R",
    package = "EHRSim"
  )
  expect_true(nzchar(code_file))

  code_env <- new.env(parent = baseenv())
  sys.source(code_file, envir = code_env)

  expect_equal(code_env$codes$measure$bmi, 1)
  expect_equal(code_env$codes$measure$weight, 2)
  expect_equal(code_env$codes$measure$height, 3)
  expect_equal(code_env$codes$entity$bmi, 36)

  bmi_codes <- unique(c(
    code_env$codes$measure$bmi,
    code_env$codes$measure$bmi_under,
    code_env$codes$measure$bmi_normal,
    code_env$codes$measure$bmi_over,
    code_env$codes$measure$bmi_obese1,
    code_env$codes$measure$bmi_obese2,
    code_env$codes$measure$bmi_obese3
  ))
  cancer_codes <- unique(c(
    code_env$codes$clinical$cancer_lung,
    code_env$codes$clinical$cancer_breast,
    code_env$codes$clinical$cancer_prostate,
    code_env$codes$clinical$cancer_bowel,
    code_env$codes$clinical$cancer_gastric,
    code_env$codes$clinical$cancer_other_m,
    code_env$codes$clinical$cancer_other_f
  ))

  expect_length(intersect(bmi_codes, cancer_codes), 0L)
})
