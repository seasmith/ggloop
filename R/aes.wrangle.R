#' Assign inputs to \code{x}, \code{y} or \code{dots}
#'
#' \code{aes_assign()} figures out which columns/variables have been
#' passed and appropriatley assigns the columns/variables to their
#' respective mapping (\code{x}, \code{y}, or \code{dots}).
#'
#' \code{aes_assign()} function is the first major function called
#' by \code{aes_loop()}.
#'
#' @param data,x,y,... Arguments passed from \code{aes_loop()}.

aes_assign <- function(data, x, y, ...){
  # set dplyr::select_vars_() variables
  vars <- names(data)
  names_list <- setNames(as.list(seq_along(vars)), vars)
  select_funs <- list(starts_with = function(...) starts_with(vars, ...),
                      ends_with = function(...) ends_with(vars, ...),
                      contains = function(...) contains(vars, ...),
                      matches = function(...) matches(vars, ...),
                      num_range = function(...) num_range(vars, ...),
                      one_of = function(...) one_of(vars, ...),
                      everything = function(...) everything(vars, ...))

  # capture x values if exist
  if(hasArg(x)){
    x <- substitute(x)
    x.eval <- lazyeval::lazy_dots(eval(x)) %>%
      lazyeval::as.lazy_dots() %>%
      lazyeval::lazy_eval(c(names_list, select_funs)) %>%
      magrittr::extract2(1) %>% vars[.] %>% list()
    is.x <- TRUE
  } else{
    x.eval <- NULL
    is.x   <- FALSE
  }

  # capture y values if exist
  if(hasArg(y)){
    y <- substitute(y)
    y.eval <- lazyeval::lazy_dots(eval(y)) %>%
      lazyeval::as.lazy_dots() %>%
      lazyeval::lazy_eval(c(names_list, select_funs)) %>%
      magrittr::extract2(1) %>% vars[.] %>% list()
    is.y <- TRUE
  } else{
    y.eval <- NULL
    is.y   <- FALSE
  }

  # capture dots if exist
  dots <- as.list(substitute(list(...)))[-1L]
  if(length(dots) > 0){
    dots.eval <- lapply(seq_along(dots), function(i){
      arg.eval <- lazyeval::lazy_dots(eval(dots[[i]])) %>%
        lazyeval::as.lazy_dots() %>%
        lazyeval::lazy_eval(c(names_list, select_funs)) %>%
        magrittr::extract2(1) %>% vars[.]
    }) %>%
      magrittr::set_names(names(dots))
    is.dots <- TRUE
  } else{
    dots.eval <- NULL
    is.dots   <- FALSE
  }

  # list logical existance and values (if any) for all arguments
  mappings <- c(is.x = is.x, x = x.eval,
                is.y = is.y, y = y.eval,
                is.dots = is.dots, dots.eval)

  return(mappings)
}

#' Create unique pairings between \code{c(x, y)} and \code{dots}.
#'
#' \code{aes_group()} uses a list of \code{x}'s and \code{y}'s
#' to create each unique combination with \code{dots}.
#'
#' @param lst A list. The list that will be passed to \code{aes_group()}
#' will be the list produced by \code{aes_assing()}.

aes_group <- function(lst){

  # MUST WRITE CODE TO DEAL WITH CIRCUMSTANCE OF NO X AND NO Y
  if(lst$is.x) rep.num <- length(lst$x) else
    if(lst$is.y) rep.num <- length(lst$y) # else
      # more code

  start <- which((names(lst) %in% "is.dots")) + 1
  end <- length(lst) - 1
  dots.vector <- start:end

  # a summary table of name, length, and order of list elements
  summ <- summary(nputs.staged) %>% as.data.frame.matrix()
  summ$Name <- rownames(summ)
  summ <- summ[ , c(4, 1:3)]

  # use summ (summary) table to find x and y
  x.pos <- which(summ$Name %in% "x")
  if(sum(x.pos) > 0) x.length <- summ$Length["x"]
  y.pos <- which(summ$Name %in% "y")
  if(sum(y.pos) > 0) y.length <- summ$Length["y"]

  xy <- c(nputs.staged$is.x*x.pos, nputs.staged$is.y*y.pos) %>%
    nputs.staged[.]

  dots.list <- lapply(unlist(lst[dots.vector]), function(x, times){
    rep(x, times)},
    times = rep.num)

  vector.len <- length(dots.vector)
  list.len <- length(dots.list)

  nlst.lst <-  lapply(seq_len(list.len/vector.len), function(x){
    unit.vector <- seq(from = 1,
                       to = list.len,
                       by = list.len/vector.len)
    itertor <- unit.vector + x - 1
    c(xy, dots.list[itertor])
  })

  return(lst)
}


#' Create unique pairings between \code{c(x, y)} and \code{dots}.
#'
#' \code{aes_group()} uses a list of \code{x's} and \code{y's}
#' to create each unique combination with \code{dots}. The difference
#' between \code{aes_group()} and \code{aes_group2()} is how they create
#' unqiue combinations. \code{aes_group2()} takes each unique \code{x},
#' \code{y} combination and assigns all \code{dots} to that unique
#' combination. \code{aes_group()} does the opposite in that it takes
#' a list of all \code{x} and \code{y} variables and assings a unique
#' \code{dots} argument. In this sense, if their are multiple variables
#' assigned to a \code{dot} (like \code{colour}, then \code{aes_group()}
#' will take a list of all \code{x} and \code{y} variables and add to it
#' \code{colour.N} where \code{.N} denotes the number of variables assigned
#' to \code{colour}.
#'
#' @param lst A list. The list that will be passed to \code{aes_group()}
#' will be the list produced by \code{aes_assing()}.

aes_group2 <- function(lst){

  # MUST WRITE CODE TO DEAL WITH CIRCUMSTANCE OF NO X AND NO Y
  if(lst$is.x) rep.num <- length(lst$x) else
    if(lst$is.y) rep.num <- length(lst$y) # else
    # more code

    start <- which((names(lst) %in% "is.dots")) + 1
    end <- length(lst) - 1
    dots.vector <- start:end

    # a summary table of name, length, and order of list elements
    summ <- summary(nputs.staged) %>% as.data.frame.matrix()
    summ$Name <- rownames(summ)
    summ <- summ[ , c(4, 1:3)]

    # use summ (summary) table to find x and y
    x.pos <- which(summ$Name %in% "x")
    if(sum(x.pos) > 0) x.length <- summ$Length["x"]
    y.pos <- which(summ$Name %in% "y")
    if(sum(y.pos) > 0) y.length <- summ$Length["y"]

    xy <- c(nputs.staged$is.x*x.pos, nputs.staged$is.y*y.pos) %>%
      nputs.staged[.] %>% matrix(unlist(.), ncol = length(.))

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
