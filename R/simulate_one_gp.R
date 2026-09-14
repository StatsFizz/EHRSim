################################################################################
#
#   Script:       simulate_one_gp.R
#
#   Purpose:      Reusable entry point for simulating EHR data for one GP
#                 practice using the existing script pipeline.
#
################################################################################

#' Simulate EHR data for one GP practice
#'
#' Runs the bundled EHR simulation pipeline for a single GP practice and
#' returns the simulated data-generating tables together with EHR-style outputs.
#'
#' @param pracid Integer practice identifier.
#' @param gp_rel_size Numeric relative size of the GP practice.
#' @param seed Integer random seed used for reproducibility.
#' @param output_dir Optional directory for saved outputs. If `NULL`, a folder
#'   is created under the current working directory.
#' @param write_csv Logical; if `TRUE`, write simulated outputs to CSV files.
#' @param overwrite Logical; if `TRUE`, allow overwriting existing output files.
#'
#' @return A list containing:
#' \describe{
#'   \item{gp}{The simulated GP-level object.}
#'   \item{tables}{A named list of simulated EHR-style tables.}
#'   \item{tables_dg}{A named list of data-generating tables.}
#'   \item{metadata}{Simulation metadata including practice ID, seed, and patient count.}
#'   \item{output_dir}{The directory used for saved outputs.}
#'   \item{output_paths}{Paths to written CSV files, if `write_csv = TRUE`.}
#' }
#' @export
simulate_one_gp <- function(pracid = 1L,
                            gp_rel_size = 10,
                            seed = 34637,
                            output_dir = NULL,
                            write_csv = TRUE,
                            overwrite = TRUE) {
  .validate_simulation_inputs(pracid, gp_rel_size, seed)
  .load_simulation_packages()

  pracid <- as.integer(pracid)
  gp_rel_size <- as.numeric(gp_rel_size)
  seed <- as.integer(seed)

  if (is.null(output_dir)) {
    output_dir <- file.path(
      getwd(),
      "Simulated_data",
      paste0("one_gp_practice_", pracid)
    )
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  sim_env <- new.env(parent = globalenv())
  sim_env$pracid <- pracid
  sim_env$gp_rel_size <- gp_rel_size
  sim_env$seed <- seed
  set.seed(seed)

  script_order <- c(
    "P0_a_Init_functions_comorbidities.R",
    "P0_b_Init_parameters_comorbidities.R",
    "P0_c_Init_function_medications.R",
    "P0_d_Init_parameters_death.R",
    "P0_e_Codes_used.R",
    "P0_f_Init_functions_treatment_effects.R",
    "P1_Simulate_GP.R",
    "P2_Simulate_patients.R",
    "P3_Simulate_frailty.R",
    "P4_Simulate_underlying_clinical_measures.R",
    "P5_a_Simulate_comorbidities_cardiometabolic.R",
    "P5_b_Simulate_comorbidities_otherdis.R",
    "P5_c_Simulate_comorbidities_dates.R",
    "P6_a_Simulate_medication_dates.R",
    "P6_b_Simulate_medications.R",
    "P7_Simulate_death_and_transfer.R",
    "P8_Update_tables_for_death_and_transfer.R",
    "P9_Simulate_consultations.R",
    "P10_Simulate_clinical_measures.R",
    "P11_Add_clinical_codes.R",
    "P12_Add_prescription_codes.R",
    "P13_Extract_EHRs.R"
  )

  pipeline_dir <- system.file("pipeline", package = "EHRSim")
  if (!nzchar(pipeline_dir)) {
    stop("Could not locate the packaged simulation pipeline.", call. = FALSE)
  }
  
  for (script_name in script_order) {
    sys.source(file.path(pipeline_dir, script_name), envir = sim_env)
  }

  output_paths <- list()
  if (isTRUE(write_csv)) {
    ehr_dir <- file.path(output_dir, "EHR_records")
    dg_dir <- file.path(output_dir, "DG_tables")

    output_paths$EHR_records <- .write_table_set(sim_env$tables, ehr_dir, overwrite = overwrite)
    output_paths$DG_tables <- .write_table_set(sim_env$tables_dg, dg_dir, overwrite = overwrite)
  }

  result <- list(
    gp = sim_env$gp,
    tables = sim_env$tables,
    tables_dg = sim_env$tables_dg,
    metadata = list(
      pracid = pracid,
      gp_rel_size = gp_rel_size,
      seed = seed,
      patient_count = nrow(sim_env$tables$patient)
    ),
    output_dir = output_dir,
    output_paths = output_paths
  )

  if (isTRUE(write_csv)) {
    message(
      "Saved one-GP simulation for practice ",
      pracid,
      " to ",
      output_dir
  )
  }

  result
}
