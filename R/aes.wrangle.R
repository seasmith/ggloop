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
#' @param x,y,dots,types,vars Arguments passed from \code{aes_loop()} or
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

aes_eval <- function(x, y, dots, types, vars) {

  # test if anything was actually passed as x or y
  x.exists <- if (is.null(x)) FALSE else TRUE
  y.exists <- if (is.null(y)) FALSE else TRUE
  xy.exists <- list(x.exists, y.exists)

  # Prepare the list of data frame names
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  # Prepare list of built-in double-dot wrapper options.
  ..something.. <- list(
    all    = types,
    number = c("integer", "numeric"),
    string = c("character", "factor")
    )

  # Function - retrieve the unevaluated x and y variables
  get_aes <- function(aes, aes.exists, types, vars, ..something..) {

    if (aes.exists) {

      # An outer c() will cause subsetting failure/confusion, as will a single
      # variable expression.
      aes <- if (is_c(aes))
        aes[-1L] else
          list(aes)

      is_dotdot <- sapply(aes, function(x) {
        grepl("^\\.\\..*\\.\\.$", x)
      })

      #
      if (any(unlist(is_dotdot))) {

        select_type <- aes %>%
          sapply(deparse) %>%
          stringi::stri_extract_all_words()


        col_type <- lapply(select_type, function(x) {

          names(..something..) %in% x %>%
            `[`(..something.., .) %>%
            unlist(use.names = FALSE) %>%
            unique()

        })

        insert_index <- !sapply(col_type, is.null)

        select_type[insert_index] <- col_type[insert_index]

        # select_index <- unlist(select_type) %T>% print()
        # select_type <- lapply(aes, function(x) {
        #   sapply(names(..something..), function(y) {
        #     identical(deparse(x), y)
        #   })
        # }) %>%
        #   sapply(which) %T>% print() %>%
        #   ..something..[.] %>%
        #   unlist()


        select_index  <- (types %in% select_type)

        return(vars[select_index])
      }

      # Need to distinguish between dplyr- and ggplot2-like calling.
      # Non-ggplot2 calling styles are assumed to be, and fall within, dplyr.
      aes.gg2   <- which_gg2(aes) %R% FALSE
      aes.dplyr <- if (isFALSE(aes.gg2))
        seq_along(aes) else
          seq_along(aes)[-aes.gg2] %R% FALSE

      # "Evaluate" both type of expression variables.
      aes.eval <- list()
      aes.eval[aes.gg2]   <- if (!isFALSE(aes.gg2)) {
        vapply(aes[aes.gg2], deparse, character(1))
      }
      aes.eval[aes.dplyr] <- if (!isFALSE(aes.dplyr)) {
        lapply(aes.dplyr, function(i) messy_eval(aes[[i]], vars, names_list))
      }

      return(unlist(aes.eval, use.names = FALSE))

    } else {

      return(NULL)

    }
  }

  # Get xy names.
  xy.eval <- Map(get_aes, list(x, y), xy.exists, list(types), list(vars), list(..something..))
  names(xy.eval) <- c("x", "y")

  ### Get dot names (if exist).
  if (length(dots)) {

    dots.eval <- list()
    dots.eval <- Map(get_aes, dots, list(TRUE), list(types), list(vars), list(..something..))
    is.dots   <- TRUE

  } else {

    dots.eval <- NULL
    is.dots   <- FALSE

  }

  # list values and logical existance of ... arguments
  mappings <- c(list(x = xy.eval[["x"]],
                     y = xy.eval[["y"]],
                     is.dots = is.dots),
                dots.eval)

  # Get rid of anything that isn't there (NULLs).
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
  env    <- new.env(parent = parent)

  xy_index <- which(names(lst) %in% c("x", "y")) %R% NA
  env$xy   <- lst[xy_index]

  if (lst[["is.dots"]]) {  # TRUE

    is.dots_index <- which(names(lst) %in% c("is.dots"))

    start <- is.dots_index + 1    # Start of dots
    end   <- length(lst)          # End of dots
    env$dots.vector <- start:end  # lst elements that are dots

    vector.len <- length(env$dots.vector)   # Number of dots aesthetics
    list.len   <- lst[env$dots.vector] %>%  # Number of total variables
      lengths() %>%
      sum()

    # Use lengths of x, y, and dots as rep number. Use max()?
    env$rep.num <- lengths(lst[stats::na.omit(c(xy_index, is.dots_index))])[1]

    # Place each dots variable in its own vector in its own list element,
    # and then rep() it env$rep.num of times.
    dots.list <- lapply(unlist(lst[env$dots.vector]),
                        function (x, times) rep(x, times),
                        times = env$rep.num)

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

  } else {                 # FALSE

    env$groups      <- env$xy
    env$dots.vector <- NULL
    env$rep.num     <- NULL

  }

  # xy, dots.vector, rep.num, groups
  return(env)

}
