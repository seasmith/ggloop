#' Helper functions to select NSE (non-standard evaluation) variable names.
#'
#' @rdname select_helpers
#'
#' @seealso
#' Source for \code{select_helpers} and the helper functions can be found at
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-vars.R}
#' and
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-utils.R}.


# current_vars() ----------------------------------------------------------

cur_vars_env <- new.env()
current_vars <- function() cur_vars_env$selected

# select_helpers() --------------------------------------------------------


select_helpers <- list(starts_with = function(...) starts_with(vars, ...),
                       ends_with = function(...) ends_with(vars, ...),
                       contains = function(...) contains(vars, ...),
                       matches = function(...) matches(vars, ...),
                       num_range = function(...) num_range(vars, ...),
                       one_of = function(...) one_of(vars, ...),
                       everything = function(...) everything(vars, ...))

# starts_with() -----------------------------------------------------------


starts_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(assertthat::is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which_vars(match, substr(vars, 1, n))
}



# ends_with() -------------------------------------------------------------


ends_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(assertthat::is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which_vars(match, substr(vars, pmax(1, length - n + 1), length))
}



# contains() --------------------------------------------------------------


contains <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(assertthat::is.string(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep_vars(match, vars, fixed = TRUE)
}



# matches() ---------------------------------------------------------------


matches <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(assertthat::is.string(match), nchar(match) > 0)

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


# non-select_helpers functions --------------------------------------------

match_vars <- function(needle, haystack) {
  x <- match(needle, haystack)
  x <- x[!is.na(x)]

  fill_out(x, haystack)
}

grep_vars <- function(needle, haystack, ...) {
  fill_out(grep(needle, haystack, ...), haystack)
}

which_vars <- function(needle, haystack) {
  fill_out(which(needle == haystack), haystack)
}

fill_out <- function(x, haystack) {
  if (length(x) > 0) return(x)
  -seq_along(haystack)
}
