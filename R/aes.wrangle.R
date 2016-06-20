
# aes_inputs2() -----------------------------------------------------------

aes_inputs2 <- function(data, x, y, ...){
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


# aes_list3() -------------------------------------------------------------

aes_split <- function(lst){
  # assuming there is either an x or y
  if(lst$is.x) rep.num <- length(lst$x) else
    if(lst$is.y) rep.num <- length(lst$y)

  start <- which((names(lst) %in% "is.dots")) + 1
  end <- length(lst) - 1
  dots.vector <- start:end

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
    dots.list[itertor]
  })
  # clean up the $names ending in a number
  return(nlst.lst)
}


# aes_inputs() ------------------------------------------------------------


# aes_inputs <- function(data, x, y, ...){
#   # capture x values if exist
#   if(hasArg(x)){
#     x <- substitute(x)
#     x.eval <- data %>% names() %>% dplyr::select_vars(eval(x)) %>%
#       unname() %>% list()
#     is.x <- TRUE
#   } else{
#     x.eval <- NULL
#     is.x   <- FALSE
#   }
#
#   #capture y values if exist
#   if(hasArg(y)){
#     y <- substitute(y)
#     y.eval <- data %>% names() %>% dplyr::select_vars(eval(y)) %>%
#       unname() %>% list()
#     is.y <- TRUE
#   } else{
#     y.eval <- NULL
#     is.y   <- FALSE
#   }
#
#   # capture dots if exist
#   dots <- as.list(substitute(list(...)))[-1L]
#   if(length(dots) > 0){
#     dots.eval <- lapply(seq_along(dots), function(i){
#       arg.eval <- data %>% dplyr::select_vars(eval(dots[[i]])) %>%
#         unname()
#     }) %>%
#       magrittr::set_names(names(dots))
#     is.dots <- TRUE
#   } else{
#     dots.eval <- NULL
#     is.dots   <- FALSE
#   }
#
#   # list logical existance and values (if any) for all arguments
#   mappings <- c(is.x = is.x, x = x.eval,
#                 is.y = is.y, y = y.eval,
#                 is.dots = is.dots, dots.eval)
#   return(mappings)
# }

# aes_list2() -------------------------------------------------------------

# aes_list2 <- function(lst){
#   # test for existence and set length to replicate dots (rep.length)
#   if(lst$is.x) rep.length <- length(lst$x)
#   if(lst$is.y) rep.length <- length(lst$y)
#   if(!lst$is.x && !lst$is.y) rep.length <- ## fill this in later
#
#       new.lst <- list()
#
#   start <- which((names(lst) %in% "is.dots")) + 1
#   end <- length(lst) - 1
#   dots <- lst[start:end]
#   lapply(seq_along(dots), function(x){
#     dots
#   })
# }

# aes_list() --------------------------------------------------------------


# aes_list <- function(lst){
#   dots.num <- length(lst) - 2
#   dots.length <- length(lst[3])
#   lapply(dots.length, function(x){
#     Map(f = aes, x = lst$x, y = lst$y, lst)
#   })
#
#   mapply(FUN = ggplot2::aes,
#          x = x,
#          y = y,
#          ... = ...,
#          SIMPLIFY = F)
# }

# aes_matrix() ------------------------------------------------------------


# aes_matrix <- function(lst){
#   # if exists(xy)
#   xy <- cbind(x = lst$x, y = lst$y)
#
#   start <- which((names(lst) %in% "is.dots")) + 1
#   end <- length(lst) - 1
#   dots <- lst[start:end] %>% unlist() %>%
#     matrix(ncol = length(start:end)) %>%
#     magrittr::set_colnames(names(lst[start:end]))
#
#   matrices <- lapply(seq_along(start:end), function(i){
#     new.dots <- dots[rep(i, dim(xy)[1]), ]
#     cbind(xy, new.dots)
#   })
#
#   return(matrices)
#
#   # if  exists(xy) one or none
# }