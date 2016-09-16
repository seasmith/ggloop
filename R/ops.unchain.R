
# `%L%+`() ----------------------------------------------------------------
#
#' Add layers to \code{ggloop} outputs
#'
#' The \code{\%L+\%} (L-plus) operator, is a \code{+} operator wrapped by an
#' \code{lapply()} loop. This function can also be a substitue for adding to
#' "raw" \code{ggplot} objects.
#'
#' \code{\%L+\%} is a substitute for \code{+} and is used in the same fashion:
#' to add geoms, stats, aesthetics, facets, and other features to \code{ggplot}
#' object. The returned object from \code{ggloop()} is often a nested list of
#' \code{ggplot} objects. However it is possible to use \code{\%L+\%} in place
#' of where \code{+} would normally be used. This is due to the conditional
#' statements present in \code{\%L+\%}'s structure.
#'
#' @param lhs Typically the returned object by \code{ggloop()}: either a nested
#'   list of \code{ggplot} objects or a list of \code{ggplot} object, but can
#'   also be a single \code{ggplot} object.
#' @param rhs A geom, stat, or other layer feature from the \code{ggplot2}
#'   package.
#'
#' @export
`%L+%` <- function(lhs, rhs) {
  parent <- parent.frame()
  env <- new.env(parent = parent)

  chain_parts <- split_chain(match.call())

  lhs_eval <- eval(chain_parts[["lhs"]], env, env)
  lhs_type <- eval_lhs(lhs_eval)

  wrapping_fun <- names(flist[which(lhs_type)])
  to_eval <- nest_function(wrapping_fun, chain_parts)

  # "Evaluate the nested function with envir = flist
  eval(to_eval, flist)
}


# split_chain() -----------------------------------------------------------
split_chain <- function(expr)
{
  # lists for holding the right-hand sides and the pipe operators.
  rhss  <- list()
  pipes <- list()

  # Process the call, splitting it at each valid %L+% operator.
  i <- 1L
  while(is.call(expr) && is_L(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]
    rhss[[i]] <- rhs

    expr <- expr[[2L]]
    i <- i + 1L
  }

  # return the components; expr will now hold the left-most left-hand side.
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}


# is_L() ------------------------------------------------------------------
is_L <- function(spec_op) identical(spec_op, quote(`%L+%`))


# eval_lhs() --------------------------------------------------------------
eval_lhs <- function(lhs) {
  # lhs = ggplot
  test_ggplot <- ggplot2::is.ggplot(lhs)

  # lhs = list()
  test_list <-
    if (test_ggplot)
      FALSE
    else
      is.list(lhs)

  # lhs = list(ggplot)
  test_list.ggplot <-
    if (test_list)
      all(vapply(lhs, ggplot2::is.ggplot, logical(1)))
    else
      FALSE

  # lhs = list(list())
  test_list.list <-
    if (test_list && !test_list.ggplot)
      all(vapply(lhs, is.list, logical(1)))
    else
      FALSE

  # lhs = list(list(ggplot))
  test_list.list.ggplot <-
    if (test_list.list)
      all(sapply(lhs, function(x) vapply(x, ggplot2::is.ggplot, logical(1))))
    else
      FALSE

  # if (any(is.na(test_list.list, test_list.list.ggplot)))
  #   stop("Left-hand side is not a list of a list of ggplot objects")

  # Create logical vector indicating which scenario - if any
  c(first = test_ggplot,
    second = all(test_list, test_list.ggplot),
    third = all(test_list.list, test_list.list.ggplot))
  }





