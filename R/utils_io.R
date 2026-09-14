#' @importFrom data.table fwrite rbindlist
#' @importFrom lubridate year years %m-%
#' @importFrom stringr str_pad
#' @importFrom survival Surv survSplit
#' @importFrom truncnorm rtruncnorm
NULL
.load_simulation_packages <- function() {
  packages <- c("truncnorm", "survival", "stringr", "data.table", "lubridate")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing required packages:",
        paste(missing, collapse = ", "),
        "\nInstall them with install.packages(c(...)) before running the simulation."
      ),
      call. = FALSE
    )
  }
  
  invisible(lapply(
    packages,
    function(package_name) {
      suppressPackageStartupMessages(
        library(package_name, character.only = TRUE)
      )
    }
  ))
}

.write_table_set <- function(table_list, output_dir, overwrite = TRUE) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  file_paths <- vapply(
    names(table_list),
    function(name) file.path(output_dir, paste0(name, "_table.csv")),
    character(1)
  )
  
  if (!overwrite && any(file.exists(file_paths))) {
    existing_files <- basename(file_paths[file.exists(file_paths)])
    stop(
      paste(
        "Refusing to overwrite existing files in",
        output_dir,
        ":",
        paste(existing_files, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(Map(
    function(name, path) {
      data.table::fwrite(table_list[[name]], file = path)
    },
    names(table_list),
    file_paths
  ))
  
  stats::setNames(as.list(file_paths), names(table_list))
}
