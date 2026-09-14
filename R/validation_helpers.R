.validate_single_number <- function(value, name, whole = FALSE,
                                    lower_bound = -Inf) {
  if (!is.numeric(value) || length(value) != 1L ||
      is.na(value) || !is.finite(value)) {
    stop(name, " must be one finite numeric value.", call. = FALSE)
  }

  if (whole && value != trunc(value)) {
    stop(name, " must be a whole number.", call. = FALSE)
  }

  if (value < lower_bound) {
    stop(name, " must be at least ", lower_bound, ".", call. = FALSE)
  }

  invisible(TRUE)
}

.validate_simulation_inputs <- function(pracid, gp_rel_size, seed) {
  .validate_single_number(pracid, "pracid", whole = TRUE, lower_bound = 1)
  .validate_single_number(gp_rel_size, "gp_rel_size", lower_bound = 0)
  if (gp_rel_size == 0) {
    stop("gp_rel_size must be greater than 0.", call. = FALSE)
  }
  .validate_single_number(seed, "seed", whole = TRUE, lower_bound = 0)

  if (pracid > .Machine$integer.max) {
    stop("pracid is too large to store as an integer.", call. = FALSE)
  }
  if (seed > .Machine$integer.max) {
    stop("seed is too large to use as an R random seed.", call. = FALSE)
  }

  invisible(TRUE)
}

.validate_gp_specs <- function(gp_specs) {
  if (!is.data.frame(gp_specs)) {
    stop("gp_specs must be a data.frame.", call. = FALSE)
  }
  if (nrow(gp_specs) == 0L) {
    stop("gp_specs must contain at least one practice.", call. = FALSE)
  }

  required_cols <- c("pracid", "gp_rel_size", "seed")
  missing_cols <- setdiff(required_cols, names(gp_specs))
  if (length(missing_cols) > 0L) {
    stop(
      "gp_specs is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(gp_specs[, required_cols, drop = FALSE])) {
    stop(
      "gp_specs cannot contain missing values in pracid, gp_rel_size, or seed.",
      call. = FALSE
    )
  }
  if (anyDuplicated(gp_specs$pracid)) {
    stop("gp_specs$pracid must be unique.", call. = FALSE)
  }

  for (i in seq_len(nrow(gp_specs))) {
    .validate_simulation_inputs(
      pracid = gp_specs$pracid[[i]],
      gp_rel_size = gp_specs$gp_rel_size[[i]],
      seed = gp_specs$seed[[i]]
    )
  }

  invisible(TRUE)
}
