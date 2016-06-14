lazyeval::lazy_dots()
lazyeval::lazy_eval()

x <- substitute(c(mpg:cyl, mpg:hp, carb))
vars <- names(mtcars)
names_list <- setNames(as.list(seq_along(vars)), vars)
args.raw <- lazyeval::lazy_dots(eval(x))
  args.raw <- lazyeval::lazy_dots(c(mpg:cyl, mpg:hp, carb))
args.as.dots <- lazyeval::as.lazy_dots(args.raw)

select_funs <- list(starts_with = function(...) starts_with(vars, ...),
                    ends_with = function(...) ends_with(vars, ...),
                    contains = function(...) contains(vars, ...),
                    matches = function(...) matches(vars, ...),
                    num_range = function(...) num_range(vars, ...),
                    one_of = function(...) one_of(vars, ...),
                    everything = function(...) everything(vars, ...))


# funs --------------------------------------------------------------------


starts_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which_vars(match, substr(vars, 1, n))
}


ends_with <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which_vars(match, substr(vars, pmax(1, length - n + 1), length))
}


contains <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep_vars(match, vars, fixed = TRUE)
}


matches <- function(match, ignore.case = TRUE, vars = current_vars()) {
  stopifnot(is.string(match), nchar(match) > 0)

  grep_vars(match, vars, ignore.case = ignore.case)
}


num_range <- function(prefix, range, width = NULL, vars = current_vars()) {
  if (!is.null(width)) {
    range <- sprintf(paste0("%0", width, "d"), range)
  }
  match_vars(paste0(prefix, range), vars)
}


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


everything <- function(vars = current_vars()) {
  seq_along(vars)
}


# final -------------------------------------------------------------------


ind_list <- lazyeval::lazy_eval(args.as.dots, c(names_list, select_funs))
vars[ind_list[[1]]]

# definitions -------------------------------------------------------------



function (vars, ..., include = character(), exclude = character())
{
  args <- lazyeval::lazy_dots(...)
  select_vars_(vars, args, include = include, exclude = exclude)
}
<environment: namespace:dplyr>
  #
  #
  #
function (vars, args, include = character(), exclude = character())
{
  if (length(args) == 0) {
    vars <- setdiff(include, exclude)
    return(setNames(vars, vars))
  }
  args <- lazyeval::as.lazy_dots(args)
  names_list <- setNames(as.list(seq_along(vars)), vars)

  select_funs <- list(starts_with = function(...) starts_with(vars, ...),
                      ends_with = function(...) ends_with(vars, ...),
                      contains = function(...) contains(vars, ...),
                      matches = function(...) matches(vars, ...),
                      num_range = function(...) num_range(vars, ...),
                      one_of = function(...) one_of(vars, ...),
                      everything = function(...) everything(vars, ...))

  ind_list <- lazyeval::lazy_eval(args, c(names_list, select_funs))
  names(ind_list) <- names2(args)
  is_numeric <- vapply(ind_list, is.numeric, logical(1))
  if (any(!is_numeric)) {
    bad_inputs <- lapply(args[!is_numeric], `[[`, "expr")
    labels <- vapply(bad_inputs, deparse_trunc, character(1))
    stop("All select() inputs must resolve to integer column positions.\n",
         "The following do not:\n", paste("* ", labels, collapse = "\n"),
         call. = FALSE)
  }
  incl <- combine_vars(vars, ind_list)
  sel <- setNames(vars[incl], names(incl))
  sel <- c(setdiff2(include, sel), sel)
  sel <- setdiff2(sel, exclude)
  if (length(sel) == 0) {
    names(sel) <- sel
  }
  else {
    unnamed <- names2(sel) == ""
    names(sel)[unnamed] <- sel[unnamed]
  }
  sel
}
<environment: namespace:dplyr>

# select ------------------------------------------------------------------

function (.data, ...)
{
  select_(.data, .dots = lazyeval::lazy_dots(...))
}
<environment: namespace:dplyr>


# select_ -----------------------------------------------------------------

function (.data, ..., .dots)
{
  UseMethod("select_")
}
<environment: namespace:dplyr>



# select_vars -------------------------------------------------------------

function (vars, ..., include = character(), exclude = character())
{
  args <- lazyeval::lazy_dots(...)
  select_vars_(vars, args, include = include, exclude = exclude)
}
<environment: namespace:dplyr>


