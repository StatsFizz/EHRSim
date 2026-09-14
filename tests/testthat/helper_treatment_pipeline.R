.treatment_test_functions <- local({
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
