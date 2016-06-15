

# ggloop() ----------------------------------------------------------------


ggloop <- function(data,
                   mapping = aes_loop(),
                   remap_xy = TRUE,
                   remap_dots = FALSE,
                   environment = parent.frame()){

}



# aes_loop() --------------------------------------------------------------


aes_loop <- function(data, x, y, ...){
  # place mapping argument values in a list
  mappings <- aes_inputs()


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(remap_xy)) remap_xy_NA(mappings)
  if(remap_xy) remap_xy_TRUE(mappings)
  if(!remap_xy) remap_xy_FALSE(mappings)

  # remap TRUE/FALSE dots
  if(is.na(remap_dots)) remap_dots_NA(mappings)
  if(remap_dots) remap_dots_TRUE(mappings)
  if(!remap_dots) remap_dots_FALSE(mappings)



}


# aes_list() --------------------------------------------------------------


aes_list <- function(lst){
  dots.num <- length(lst) - 2
  dots.length <- length(lst[3])
  lapply(dots.length, function(x){
    Map(f = aes, x = lst$x, y = lst$y, lst)
  })

  mapply(FUN = ggplot2::aes,
         x = x,
         y = y,
         ... = ...,
         SIMPLIFY = F)
}


# aes_inputs() ------------------------------------------------------------


aes_inputs <- function(data, x, y, ...){
  # capture x values if exist
  if(hasArg(x)){
    x <- substitute(x)
    x.eval <- data %>% names() %>% dplyr::select_vars(eval(x)) %>%
      unname() %>% list()
    is.x <- TRUE
  } else{
    x.eval <- NULL
    is.x   <- FALSE
  }

  #capture y values if exist
  if(hasArg(y)){
    y <- substitute(y)
    y.eval <- data %>% names() %>% dplyr::select_vars(eval(y)) %>%
      unname() %>% list()
    is.y <- TRUE
  } else{
    y.eval <- NULL
    is.y   <- FALSE
  }

  # capture dots if exist
  dots <- as.list(substitute(list(...)))[-1L]
  if(length(dots) > 0){
    dots.eval <- lapply(seq_along(dots), function(i){
      arg.eval <- data %>% dplyr::select_vars(eval(dots[[i]])) %>%
        unname()
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

  #capture y values if exist
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

