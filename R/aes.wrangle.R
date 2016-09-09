#' @include utilities.eval.R

# aes_eval() ----------------------------------------------------------
#
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

aes_eval <- function(vars, x, y, dots){

  # test if anything was actually passed as x or y
  x.exists <- tryCatch({
    get0("x")
    TRUE
    }, error = function(e){
      FALSE
    })

  y.exists <- tryCatch({
    get0("y")
    TRUE
  }, error = function(e){
    FALSE
  })

  names_list <- setNames(as.list(seq_along(vars)), vars)

  # --- Capture x values if x exists
  if(x.exists){
    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    x <- if(is.symbol(x)){list(x)} else{
              if(is.c(x[[1L]])){x[-1L]} else{
                list(x)
              }
      }

    x.eval <- list()

    # Evaluate entries with ggplot2-like syntax.
    rm <- rm.gg2(x) %||% FALSE

      x.eval[abs(rm)] <- if(!isFALSE(rm)){
        sapply(x[abs(rm)], deparse)
      }

    # Evaluate entries with dplyr-like syntax (assumed).
    kp <- if(isFALSE(rm)){
      seq_along(x)
      } else{
        seq_along(x)[rm] %||% FALSE
      }

      x.eval[kp] <- if(!isFALSE(kp)){
        lapply(kp, function(i){
        messy_eval(x[[i]], vars, names_list)
          })
        }

    x.eval <- unlist(x.eval, use.names = FALSE)
    } else{
      x.eval <- NULL
      }

  # --- Capture y values if y exists.
  if(y.exists){
    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    y <- if(is.symbol(y)){list(y)} else{
      if(is.c(y[[1L]])){y[-1L]} else{
        list(y)
      }
    }

    y.eval <- list()

      # Evaluate entries with ggplot2-like syntax.
      rm <- rm.gg2(y) %||% FALSE

        y.eval[abs(rm)] <- if(!isFALSE(rm)){
          sapply(y[abs(rm)], deparse)
        }

      # Evaluate entries with (assumed) dplyr-like syntax.
      kp <- if(isFALSE(rm)){
        seq_along(y)
      } else{
        seq_along(y)[rm] %||% FALSE
        }

        y.eval[kp] <- if(!isFALSE(kp)){
          lapply(kp, function(i){
            messy_eval(y[[i]], vars, names_list)
          })
        }

    y.eval <- unlist(y.eval, use.names = FALSE)
    } else{
      y.eval <- NULL
      }

  # --- Capture dots if exist
  if(length(dots) > 0){
    # Capture names (names will be lost in the following lapply()).
    dots.names <- names(dots)
    # Strip c().
    dots <- lapply(seq_along(dots), function(x){
      if(is.c(dots[[x]][[1L]])) dots[[x]][-1L]
      else list(dots[[x]])
    })

    # Creat list to hold evaluations.
    dots.eval <- list()

      # Remove and Keep
      rm <- sapply(dots, function(x){
        rm.gg2(x) %||% FALSE
      })

        # Evaluate dplyr-like expressions.
        rm.eval <- lapply(seq_along(dots), function(i){
          d.eval <- list()
          d.eval[rev(abs(rm[[i]]))] <- sapply(
            dots[[i]][rev(abs(rm[[i]]))], deparse) %||% NULL
        })


      kp <- lapply(seq_along(dots), function(i){
        if(isFALSE(rm[[i]])){
          seq_along(dots[[i]])
        } else{
          seq_along(dots[[i]])[rm[[i]]] %||% FALSE
        }
      })

        # Evaluate ggplot2-like expressions.
        kp.eval <- lapply(seq_along(kp), function(i){
          if(isFALSE(kp[[i]])) NULL
          else {
            d.eval <- list()
            d.eval[kp[[i]]] <- lapply(
              kp[[i]], function(j) messy_eval(dots[[i]][[j]], vars, names_list))
            d.eval
          }
        })


    # Combine Evaluations
    dots.eval <- lapply(
      seq_along(dots), function(x) c(unlist(rm.eval[[x]]), unlist(kp.eval[[x]])))
    names(dots.eval) <- dots.names
    is.dots <- TRUE
  } else{
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
#' Create unique pairings between \code{x}, \code{y} and \code{dots}.
#'
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

aes_group <- function(lst){
  ee <- new.env()
    # Variables that will be stashed in ee:
      # xy, dots.vector, rep.num, groups

  xy <- lst[na.omit(c(list.pos("x", lst), list.pos("y",lst)))]
    # stash
    ee$xy <- xy

  if(!lst[["is.dots"]]){
    groups <- xy
      # stash
      ee$dots.vector <- NULL
      ee$rep.num <- NULL
  } else{
    start <- list.pos("is.dots", lst) + 1
    end <- length(lst)
    # stash
    ee$dots.vector <- start:end

    # might need to use max()
    # stash
    ee$rep.num <- lengths(lst[na.omit(c(list.pos("x", lst),
                                      list.pos("y", lst),
                                      list.pos("is.dots", lst)))])[1]

    dots.list <- lapply(unlist(lst[ee$dots.vector]),
                        function(x, times) rep(x, times),
                        times = ee$rep.num)

    vector.len <- length(ee$dots.vector)
    list.len <- length(dots.list)

    ee$groups <-  lapply(seq_len(list.len/vector.len), function(x){
      unit.vector <- seq(from = 1,
                         to = list.len,
                         by = list.len/vector.len)
      iterator <- unit.vector + x - 1
      c(xy, dots.list[iterator])
    })
  }

  return(ee)
}
