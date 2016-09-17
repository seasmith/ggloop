#' @include utilities.eval.R

# aes_eval() ----------------------------------------------------------
#' @title
#' Assign inputs to \code{x}, \code{y} or \code{dots}.
#'
#' @description
#' \code{aes_eval()} figures out which variables have been passed and
#' appropriatley assigns the variables to their respective mapping: either
#' (\code{x}, \code{y}, or \code{dots}). Furthermore, it distinguishes between
#' ggplot-like syntax and dplyr-like syntax calling of variables.
#'
#'
#' @param vars,x,y,dots Arguments passed from \code{aes_loop()} or
#'   \code{aes_loop2()}.
#'
#' @details
#' \code{aes_eval()} is the first major function to be called by
#' \code{aes_loop()}.
#'
#' @return
#' The list returned by \code{aes_eval()} is the input for the remapping
#' functions.
#'
#' The logical vector \code{$is.dots} is placed between the \code{x} and
#' \code{y} vectors (if any) and the \code{dots} vectors (if any). This is used
#' for easy reference in \code{if} statements.
#'
#' The length of each vector (\code{x}, \code{y}, and \code{dots}) in the output
#' list is determined by the length of the vector passed to \code{aes_loop()}.
#' If an \code{x} or \code{y} variable is passed more than once, then it will be
#' present in the vector the same number of times it was passed into
#' \code{aes_loop()}.
#'
#' @seealso
#' Source for \code{names_list} and code structure of \code{lazyeval::} function
#' calls can be found at
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-vars.R}
#' and
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-utils.R}.

aes_eval <- function(vars, x, y, dots) {

  # test if anything was actually passed as x or y
  x.exists <- tryCatch({
    get0("x")
    TRUE
    }, error = function(e) {
      FALSE
    })

  y.exists <- tryCatch({
    get0("y")
    TRUE
  }, error = function(e) {
    FALSE
  })

  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  ### Capture x values if x exists
  if (x.exists) {
    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    x <- if (is.c(x)) x[-1L] else list(x)

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    x.gg2   <- rm.gg2(x) %R% FALSE
    x.dplyr <- if (isFALSE(x.gg2)) seq_along(x) else {
      seq_along(x)[-x.gg2] %R% FALSE
    }

    # "Evaluate" both type of expression variables.
    x.eval <- list()
    x.eval[x.gg2]   <- if (!isFALSE(x.gg2)) sapply(x[x.gg2], deparse)
    x.eval[x.dplyr] <- if (!isFALSE(x.dplyr)) {
      lapply(x.dplyr, function(i) messy_eval(x[[i]], vars, names_list))
      }

    x.eval <- unlist(x.eval, use.names = FALSE)
    } else {
      x.eval <- NULL
      }

  ### Capture y values if y exists.
  if (y.exists) {
    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    y <- if (is.c(y)) y[-1L] else list(y)

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    y.ggplot2 <- rm.gg2(y) %R% FALSE
    y.dplyr   <- if (isFALSE(y.ggplot2)) seq_along(y) else {
      seq_along(y)[-y.ggplot2] %R% FALSE
    }

    # "Evaluate" both type of expression variables.
    y.eval <- list()
    y.eval[y.ggplot2] <- if (!isFALSE(y.ggplot2)) sapply(y[y.ggplot2], deparse)
    y.eval[y.dplyr]   <- if (!isFALSE(y.dplyr)) {
      lapply(y.dplyr, function(i) messy_eval(y[[i]], vars, names_list))
    }

    y.eval <- unlist(y.eval, use.names = FALSE)
    } else {
      y.eval <- NULL
      }

  ### Capture dots if exist
  if (length(dots)) {
    # Capture names (names will be lost in the following lapply()'s).
    dots.names <- names(dots)

    # Strip c().
    dots <- lapply(seq_along(dots), function(x) {
      if (is.c(dots[[x]])) dots[[x]][-1L] else list(dots[[x]])
    })

    # Creat list to hold evaluations.
    dots.eval <- list()

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    dots.gg2   <- lapply(dots, function(x) rm.gg2(x) %R% FALSE)
    dots.dplyr <- lapply(seq_along(dots), function(i) {
      if (isFALSE(dots.gg2[[i]])) seq_along(dots[[i]])
      else seq_along(dots[[i]])[-dots.gg2[[i]]] %R% FALSE
    })

    # "Evaluate" both types of expressions.
    dots.gg2.eval   <- lapply(seq_along(dots), function(i) {
      d.eval <- list()
      d.eval[rev(abs(dots.gg2[[i]]))] <- sapply(
        dots[[i]][rev(dots.gg2[[i]])], deparse) %R% NULL
    })
    dots.dplyr.eval <- lapply(seq_along(dots.dplyr), function(i) {
      if (isFALSE(dots.dplyr[[i]])) NULL
      else {
        d.eval <- list()
        d.eval[dots.dplyr[[i]]] <- lapply(dots.dplyr[[i]], function(j) {
          messy_eval(dots[[i]][[j]], vars, names_list)
          })
        d.eval
      }
    })

    # Combine "Evaluations"
    dots.eval <- lapply(seq_along(dots), function(x) {
      c(unlist(dots.gg2.eval[[x]]), unlist(dots.dplyr.eval[[x]]))
      })

    names(dots.eval) <- dots.names
    is.dots <- TRUE
  } else {
    dots.eval <- NULL
    is.dots   <- FALSE
  }
  # list values and logical existance of ... arguments
  mappings <- c(list(x = x.eval,
                     y = y.eval,
                     is.dots = is.dots),
                dots.eval)

  mappings <- mappings[!vapply(mappings, is.null, logical(1))]

  return(mappings)
}


# aes_group() -------------------------------------------------------------
#
#' @title
#' Create unique pairings between \code{x}, \code{y} and \code{dots}.
#'
#' @description
#' \code{aes_group()} uses a list of \code{x}'s and \code{y}'s to create each
#' unique combination with \code{dots}.
#'
#' @param lst A list. The list that will be passed to \code{aes_group()} will be
#'   the list produced by \code{aes_assing()}.
#'
#' @details \code{aes_group()} uses an {lapply} loop to give every \code{dots}
#' element with a copy of the \code{x} and \code{y} vectors (if any). This
#' creates a list in which the first set of components correspond to the
#' combination of \code{dots} elements, and the second set of components (the
#' nested components) correspond to the \code{x} and \code{y} vectors.

aes_group <- function(lst) {
  parent <- parent.frame()
  env <- new.env(parent = parent)

  env$xy <- lst[stats::na.omit(c(list.pos("x", lst), list.pos("y",lst)))]

  if (lst[["is.dots"]]) {
    start <- list.pos("is.dots", lst) + 1
    end <- length(lst)
    env$dots.vector <- start:end

    # might need to use max()
    env$rep.num <- lengths(lst[stats::na.omit(c(list.pos("x", lst),
                                               list.pos("y", lst),
                                               list.pos("is.dots", lst)))])[1]

    dots.list <- lapply(unlist(lst[env$dots.vector]),
                        function (x, times) rep(x, times),
                        times = env$rep.num)

    vector.len <- length(env$dots.vector)
    list.len <- length(dots.list)

    # Group xy and dots, and then rename (scrape off the trailing numbers in the
    # dots)
    env$groups <-  lapply(seq_len(list.len/vector.len), function(x) {
      unit.vector <- seq(from = 1,
                         to = list.len,
                         by = list.len/vector.len)
      iterator <- unit.vector + x - 1
      c(env$xy, dots.list[iterator])
    })
    env$groups <- rename_inputs(env$groups)
  } else {
    env$groups <- env$xy
    env$dots.vector <- NULL
    env$rep.num <- NULL
  }
  # xy, dots.vector, rep.num, groups
  return(env)
}
