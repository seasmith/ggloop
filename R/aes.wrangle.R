
# aes_eval() ----------------------------------------------------------
#
#' Assign inputs to \code{x}, \code{y} or \code{dots}
#'
#' \code{aes_eval()} figures out which variables have been passed and
#' appropriatley assigns the variables to their respective mapping: either
#' (\code{x}, \code{y}, or \code{dots}).
#'
#' \code{aes_eval()} function is the first major function called by
#' \code{aes_loop()}.
#'
#' @param vars,x,y,dots Arguments passed from \code{aes_loop()} or
#'   \code{aes_loop2()}.
#'
#' @details The length of each vector (\code{x}, \code{y}, and \code{dots}) is
#' determined by the length of the vector passed to \code{aes_loop()}. If an
#' \code{x} or \code{y} variable is passed more than once, then it will be
#' present however many times it was passed.
#'
#' The logical vector \code{$is.dots} is placed between the \code{x} and
#' \code{y} vectors (if any) and the \code{dots} vectors (if any). This is used
#' for easy reference in \code{if} statements.
#'
#' The list returned by \code{aes_eval()} is the input for the remapping
#' functions for \code{x}, \code{y}, and \code{dots}.
#'
#' @seealso
#' Source for \code{names_list} and code structure of \code{lazyeval::} function
#' calls can be found at
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-vars.R}
#' and
#' \href{https://github.com/hadley/dplyr/blob/master/R/select-utils.R}{~/dplyr/R/select-utils.R}.

aes_eval <- function(vars, x, y, dots){
  names_list <- setNames(as.list(seq_along(vars)), vars)

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

  # capture x values if x exists
  if(x.exists){
    # # Strip c() wrapper or wrap in list if no c() (for is.fun())
    # if(is.c(x[[1L]])) x <- x[-1L]
    # else x <- list(x)
    #
    # # "Remove" entries with ggplot2-like syntax.
    # # "Keep" other entries (assumed to have dplyr-like syntax)
    # rm <- rm.gg2(x) %||% FALSE
    # kp <- if(isFALSE(rm)){
    #   seq_along(x)
    # } else{
    #   seq_along(x)[rm] %||% FALSE
    # }
    #
    # x.eval <- list()
    #
    # x.eval[kp] <- if(length(kp)){
    #   lapply(kp, function(i){
    #   messy_eval(x[[i]], vars, names_list)
    # })
    # }
    #
    # x.eval[abs(rm)] <- if(length(rm)){
    #   sapply(x[abs(rm)], deparse)
    # }

    rm <- (rm.gg2(x[-1L]) - 1)
    kp <- if(length(rm) > 0){
            seq_along(x)[rm][-1L]
          } else{
              seq_along(x)[-1L]
          }

    x.eval <- list()
    x.eval[kp - 1] <- lapply(kp, function(i){
      messy_eval(x[c(1, i)], vars, names_list)
      })
    if(length(rm) > 0) x.eval[-(rm + 1)] <- sapply(x[-rm], deparse)

    x.eval <- unlist(x.eval) %>% `names<-`(NULL)
  } else{
    x.eval <- NULL
  }

  # capture y values if y exists
  if(y.exists){
    # # Strip c() wrapper or wrap in list if no c() (for is.fun())
    # if(is.c(y[[1L]])) y <- y[-1L]
    # else y <- list(y)
    #
    # # "Remove" entries with ggplot2-like syntax.
    # # "Keep" other entries (assumed to have dplyr-like syntax)
    # rm <- rm.gg2(y) %||% FALSE
    # kp <- if(isFALSE(rm)){
    #   seq_along(y)
    # } else{
    #   seq_along(y)[rm] %||% FALSE
    # }
    #
    # y.eval <- list()
    #
    # y.eval[kp] <- if(length(kp)){
    #   lapply(kp, function(i){
    #   messy_eval(y[[i]], vars, names_list)
    # })
    # }
    #
    # y.eval[abs(rm)] <- if(length(rm)){
    #   sapply(y[abs(rm)], deparse)
    # }

    rm <- (rm.gg2(y[-1L]) - 1)
    kp <- if(length(rm) > 0){
            seq_along(y)[rm][-1L]
          } else{
            seq_along(y)[-1L]
          }

    y.eval <- list()
    y.eval[kp - 1] <- lapply(kp, function(i){
      messy_eval(y[c(1, i)], vars, names_list)
      })
    if(length(rm) > 0) y.eval[-(rm + 1)] <- sapply(y[-rm], deparse)

    y.eval <- unlist(y.eval) %>% `names<-`(NULL)
  } else{
    y.eval <- NULL
  }

  # capture dots if exist
  if(length(dots) > 0){
    rm <- rm.gg2(dots)
    kp <- if(length(rm) > 0){
            seq_along(dots)[rm]
          } else{
            seq_along(dots)
          }

    dots.eval <- list()
    dots.eval <- sapply(seq_along(kp), function(i){
      messy_eval(dots[[i]], vars, names_list)
    })
    if(length(rm) > 0) dots.eval <- sapply(dots[-rm], deparse)

    names(dots.eval) <- names(dots)
    is.dots <- TRUE
  } else{
    dots.eval <- NULL
    is.dots   <- FALSE
  }
print(dots.eval)
  # list values and logical existance of ... arguments
  mappings <- c(x = list(x.eval),
                y = list(y.eval),
                is.dots = is.dots,
                dots.eval)
rtn <<- mappings
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
  # ee <- new.env()
  pf <- parent.frame()

  xy <- lst[na.omit(c(list.pos("x", lst), list.pos("y",lst)))]
    # stash
    # ee$xy <- xy
    # send to parent
    pf$xy <- xy

  if(!lst[["is.dots"]]){
    groups <- xy
      # stash
      # ee$dots.vector <- NULL
      # ee$rep.num <- NULL
      # send to parent
      pf$dots.vector <- NULL
      pf$rep.num <- NULL
  } else{
    start <- list.pos("is.dots", lst) + 1
    end <- length(lst)
    dots.vector <- start:end
      # stash
      # ee$dots.vector <- dots.vector
      # send to parent
      pf$dots.vector <- dots.vector

    # might need to use max()
    rep.num <- lengths(lst[na.omit(c(list.pos("x", lst),
                                      list.pos("y", lst),
                                      list.pos("is.dots", lst)))])[1]
      # stash
      # ee$rep.num <- rep.num
      # send to mother
      pf$rep.num <- rep.num

    dots.list <- lapply(unlist(lst[dots.vector]),
                        function(x, times) rep(x, times),
                        times = rep.num)

    vector.len <- length(dots.vector)
    list.len <- length(dots.list)

    groups <-  lapply(seq_len(list.len/vector.len), function(x){
      unit.vector <- seq(from = 1,
                         to = list.len,
                         by = list.len/vector.len)
      iterator <- unit.vector + x - 1
      c(xy, dots.list[iterator])
    })
  }
  # stash
  # ee$groups <- groups
  # return(ee)
  return(groups)
}


# aes_group2() ------------------------------------------------------------
#
#' Create unique pairings between \code{c(x, y)} and \code{dots}.
#'
#' \code{aes_group()} uses a list of \code{x's} and \code{y's} to create each
#' unique combination with \code{dots}. The difference between
#' \code{aes_group()} and \code{aes_group2()} is how they create unqiue
#' combinations. \code{aes_group2()} takes each unique \code{x}, \code{y}
#' combination and assigns all \code{dots} to that unique combination.
#'
#' \code{aes_group()} does the opposite in that it takes a list of all \code{x}
#' and \code{y} variables and assings a unique \code{dots} argument. In this
#' sense, if there are multiple variables assigned to a \code{dot} (like
#' \code{colour}, then \code{aes_group()} will take a list of all \code{x} and
#' \code{y} variables and add to it \code{colour.N} where \code{.N} denotes the
#' number of variables assigned to \code{colour}.
#'
#' @param lst A list. The list that will be passed to \code{aes_group()} will be
#'   the list produced by \code{aes_assing()}.

aes_group2 <- function(lst){

  # MUST WRITE CODE TO DEAL WITH CIRCUMSTANCE OF NO X AND NO Y
  if(lst$is.x) rep.num <- length(lst$x) else
    if(lst$is.y) rep.num <- length(lst$y) # else
    # more code

    start <- which((names(lst) %in% "is.dots")) + 1
    end <- length(lst) - 1
    dots.vector <- start:end

    # a summary table of name, length, and order of list elements
    summ <- summary(lst) %>% as.data.frame.matrix()
    summ$Name <- rownames(summ)
    summ <- summ[ , c(4, 1:3)]

    # use summ (summary) table to find x and y
    x.pos <- which(summ$Name %in% "x")
    if(sum(x.pos) > 0) x.length <- summ$Length["x"]
    y.pos <- which(summ$Name %in% "y")
    if(sum(y.pos) > 0) y.length <- summ$Length["y"]

    xy <- c(lst$is.x*x.pos, lst$is.y*y.pos) %>%
      lst[.] %>% matrix(unlist(.), ncol = length(.))

    dots.list <- lapply(unlist(lst[dots.vector]), function(x, times){
      rep(x, times)},
      times = rep.num)

    # vector.len <- length(dots.vector)
    # list.len <- length(dots.list)

    # nlst.lst <-  lapply(seq_len(list.len/vector.len), function(x){
    #   unit.vector <- seq(from = 1,
    #                      to = list.len,
    #                      by = list.len/vector.len)
    #   itertor <- unit.vector + x - 1
    #   c(xy[iterator], dots.list)
    # })



    return(lst)
}
