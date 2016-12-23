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

aes_eval <- function(aesth, vars) {

  # UseMethod(xyz)

  # test if anything was actually passed as x or y
  x.exists <- if (is.null(aesth$x)) FALSE else TRUE
  y.exists <- if (is.null(aesth$y)) FALSE else TRUE
  z.exists <- if (is.null(aesth$z)) FALSE else TRUE
  xyz.exists <- list(x.exists, y.exists, z.exists)

  # Prepare the list of data frame names
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  # Function - retrieve the actual x, y, and z unevaluted names; should appear
  # in aes_eval.dots() and aes_eval.nodots() methods.
  get_xyz <- function(aesth, aesth.exists, names) {

    if (aesth.exists) {

    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    aesth <- if (is_c(aesth)) aesth[-1L] else list(aesth)

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    aesth.gg2   <- rm_gg2(aesth) %R% FALSE
    aesth.dplyr <- if (isFALSE(aesth.gg2)) seq_along(aesth) else {
      seq_along(aesth)[-aesth.gg2] %R% FALSE
    }

    # "Evaluate" both type of expression variables.
    aesth.eval <- list()
    aesth.eval[aesth.gg2]   <- if (!isFALSE(aesth.gg2)) vapply(aesth[aesth.gg2], deparse, character(1))
    aesth.eval[aesth.dplyr] <- if (!isFALSE(aesth.dplyr)) {
      lapply(aesth.dplyr, function(i) messy_eval(aesth[[i]], vars, names_list))
    }

    return(unlist(aesth.eval, use.names = FALSE))
    } else {
      return(NULL)
      }
    }

  # These two lines should also appear in both aes_eval methods.
  xyz.eval <- Map(get_xyz, aesth[c("x", "y", "z")], xyz.exists, list(vars))
  names(xyz.eval) <- c("x", "y", "z")

  # Rewrite as function and place ONLY in aes_eval.dots() method
  ### Capture dots if exist
  if (length(dots)) {
    # Capture names (names will be lost by the dots.dplyr.eval lapply()).
    dots.names <- names(dots)

    # Strip c().
    dots <- lapply(dots, function(x) if (is_c(x)) x[-1L] else list(x))

    # Creat list to hold evaluations.
    dots.eval <- list()

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    dots.gg2   <- lapply(dots, function(x) rm_gg2(x) %R% FALSE)
      find.dplyr <- function(x, y) {
        if (isFALSE(y)) seq_along(x)
        else seq_along(x)[-y] %R% FALSE
      }
    dots.dplyr <- Map(find.dplyr, x = dots, y = dots.gg2)

    # "Evaluate" both types of expressions.
      eval.gg2 <- function(x, y) {
        d.eval <- list()
        d.eval[rev(abs(y))] <- vapply(x[rev(y)], deparse, character(1)) %R% NULL
      }
    dots.gg2.eval   <- Map(eval.gg2, x = dots, y = dots.gg2)
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
  mappings <- c(list(x = x.eval, y = y.eval, is.dots = is.dots),
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
    end   <- length(lst)
    env$dots.vector <- start:end

    # might need to use max()
    env$rep.num <- lengths(lst[stats::na.omit(c(list.pos("x", lst),
                                               list.pos("y", lst),
                                               list.pos("is.dots", lst)))])[1]

    dots.list <- lapply(unlist(lst[env$dots.vector]),
                        function (x, times) rep(x, times),
                        times = env$rep.num)

    vector.len <- length(env$dots.vector)
    list.len   <- length(dots.list)

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
    env$groups      <- env$xy
    env$dots.vector <- NULL
    env$rep.num     <- NULL
  }
  # xy, dots.vector, rep.num, groups
  return(env)
}
