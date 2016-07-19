# The functions below were taken from ggplot2/R/select-utils.R, and can be
# found at the following GitHub link:
# https://raw.githubusercontent.com/hadley/dplyr/master/R/select-utils.R

# starts_with() -----------------------------------------------------------


starts_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which_vars(match, substr(vars, 1, n))
}



# ends_with() -------------------------------------------------------------


ends_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which_vars(match, substr(vars, pmax(1, length - n + 1), length))
}



# contains() --------------------------------------------------------------


contains <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep_vars(match, vars, fixed = TRUE)
}



# matches() ---------------------------------------------------------------


matches <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  grep_vars(match, vars, ignore.case = ignore.case)
}



# num_range() -------------------------------------------------------------


num_range <- function(prefix, range, width = NULL, vars = current_vars()) {
  if (!is.null(width)) {
    range <- sprintf(paste0("%0", width, "d"), range)
  }
  match_vars(paste0(prefix, range), vars)
}



# one_of() ----------------------------------------------------------------


one_of <- function(..., vars = current_vars()) {
  keep <- c(...)

  if (!is.character(keep)) {
    stop("`c(...)` must be a character vector", call. = FALSE)
  }

  if (!all(keep %in% vars)) {
    bad <- setdiff(keep, vars)
    warning("Unknown variables: ", paste0("`", bad, "`", collapse = ", "))
  }

  match_vars(keep, vars)
}



# everything() ------------------------------------------------------------


everything <- function(vars = current_vars()) {
  seq_along(vars)
}
