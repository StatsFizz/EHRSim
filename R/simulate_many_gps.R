################################################################################
#
#   Script:       simulate_many_gps.R
#
#   Purpose:      Reusable entry point for simulating EHR data for many GP
#                 practices using the existing script for simulating one gp.
#
################################################################################
#' Simulate EHR data for multiple GP practices
#'
#' Runs the bundled EHR simulation pipeline repeatedly for a set of GP
#' practices and combines the resulting outputs.
#'
#' @param gp_specs A data frame containing one row per GP practice with columns
#'   `pracid`, `gp_rel_size`, and `seed`.
#' @param output_dir Optional directory for saved outputs. If `NULL`, a folder
#'   is created under the current working directory.
#' @param write_csv Logical; if `TRUE`, write simulated outputs to CSV files.
#' @param overwrite Logical; if `TRUE`, allow overwriting existing output files.
#' @param keep_gp_results Logical; if `TRUE`, retain the individual GP-level
#'   results inside the returned object.
#' @param save_mode Character string indicating whether to save `"combined"`,
#'   `"per_gp"`, or `"both"` outputs.
#'
#' @return A list containing:
#' \describe{
#'   \item{gp_specs}{The input GP specification table.}
#'   \item{tables}{A named list of combined EHR-style tables.}
#'   \item{tables_dg}{A named list of combined data-generating tables.}
#'   \item{metadata}{Simulation metadata including number of practices and patients.}
#'   \item{output_dir}{The directory used for saved outputs.}
#'   \item{output_paths}{Paths to written CSV files, if applicable.}
#'   \item{gp_results}{Individual GP-level results if `keep_gp_results = TRUE`, otherwise `NULL`.}
#' }
#' @export
simulate_many_gps <- function(gp_specs,
                              output_dir = NULL,
                              write_csv = TRUE,
                              overwrite = TRUE,
                              keep_gp_results = FALSE,
                              save_mode = "both") {
  .validate_gp_specs(gp_specs)
  .load_simulation_packages()

  if (!is.character(save_mode) || length(save_mode) != 1L ||
      is.na(save_mode) || !save_mode %in% c("combined", "per_gp", "both")) {
    stop("save_mode must be one of: 'combined', 'per_gp', 'both'", call. = FALSE)
  }
  
  
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "Simulated_data", "many_gps")
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  
  gp_results <- vector("list", nrow(gp_specs))
  
  write_per_gp <- isTRUE(write_csv) && save_mode %in% c("per_gp", "both")
  
  for (i in seq_len(nrow(gp_specs))) {
    this_output_dir <- file.path(output_dir, paste0("practice_", gp_specs$pracid[i]))
    
    gp_results[[i]] <- simulate_one_gp(
      pracid = as.integer(gp_specs$pracid[i]),
      gp_rel_size = as.numeric(gp_specs$gp_rel_size[i]),
      seed = as.integer(gp_specs$seed[i]),
      output_dir = this_output_dir,
      write_csv = write_per_gp,
      overwrite = overwrite
    )
  }
  
  table_names <- names(gp_results[[1]]$tables)
  dg_table_names <- names(gp_results[[1]]$tables_dg)
  
  combined_tables <- stats::setNames(
    lapply(table_names, function(tbl_name) {
      data.table::rbindlist(
        lapply(gp_results, function(res) res$tables[[tbl_name]]),
        use.names = TRUE,
        fill = TRUE
      )
    }),
    table_names
  )
  
  combined_tables_dg <- stats::setNames(
    lapply(dg_table_names, function(tbl_name) {
      data.table::rbindlist(
        lapply(gp_results, function(res) res$tables_dg[[tbl_name]]),
        use.names = TRUE,
        fill = TRUE
      )
    }),
    dg_table_names
  )
  
  output_paths <- list()
  write_combined <- isTRUE(write_csv) && save_mode %in% c("combined", "both")
  
  if (write_combined) {
    ehr_dir <- file.path(output_dir, "EHR_records")
    dg_dir <- file.path(output_dir, "DG_tables")
    
    output_paths$EHR_records <- .write_table_set(combined_tables, ehr_dir, overwrite = overwrite)
    output_paths$DG_tables <- .write_table_set(combined_tables_dg, dg_dir, overwrite = overwrite)
  }
  
  result <- list(
    gp_specs = gp_specs,
    tables = combined_tables,
    tables_dg = combined_tables_dg,
    metadata = list(
      n_gps = nrow(gp_specs),
      total_patients = nrow(combined_tables$patient),
      save_mode = save_mode
    ),
    output_dir = output_dir,
    output_paths = output_paths,
    gp_results = if (isTRUE(keep_gp_results)) gp_results else NULL
  )
  
  if (isTRUE(write_csv)) {
    message(
      "Saved many-GP simulation for ",
      nrow(gp_specs),
      " practices to ",
      output_dir,
      " using save_mode = ",
      save_mode
    )
  }
  
  result
}
