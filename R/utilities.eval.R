
# ops ---------------------------------------------------------------------
#
#' Arithmetic operators to search for.

ops <- c("/", "\\+", "-", "\\*", "\\^")


# is.arth() ---------------------------------------------------------------
#
#' Determine if an input uses an arithmetical operator (\code{/}, \code{+},
#' \code{-}, \code{*}, \code{^}).

is.arth <- function(lst){
  has.ops <- sapply(ops, function(x) grep(x, lst))
  has.ops <- unlist(has.ops)
  names(has.ops) <- NULL
  unique(has.ops)
}


# fun.par -----------------------------------------------------------------
#
#' Regular expression pattern for determing if possible function parenthesis
#' are present. Searches for \code{"("} and \code{")"} preceeded by any number
#' of characters.

fun.par <- c("[A-Za-z]+\\(.+\\)")


# is.c() ------------------------------------------------------------------
#
#' Determine if the supplied input is identical to the \code{c} function.
#' @param x Possibly the body of a function.

is.c <- function(x) identical(x, c)


# is.fun() ----------------------------------------------------------------
#
#' Attempts to decipher if a function other than \code{c()} has been supplied as
#' input. Returns the position of the possible non-\code{c} functions in
#' \code{lst}.
#'
#' @param lst A list of inputs wrapped in \code{substitute()} and coerced to a
#' list using \code{as.list()}.

is.fun <- function(lst){
  pars <- lapply(fun.par, function(x) grep(x, lst))[[1L]]
  is.c.TRUE <- sapply(lst[pars], function(x){
    x.parse <- parse(text = x)
    x.eval <- eval(x.parse[[1L]])
    is.c(x.eval)
    })
  pars[!is.c.TRUE]
}

tst <- function(...){
  dots <- as.list(substitute(list(...)))[-1L]
}
