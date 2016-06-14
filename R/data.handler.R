

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
  if(remap_xy) remap_xy_TRUE(mapping)
  if(!remap_xy) remap_xy_FALSE(mapping)
  if(is.na(remap_xy)) remap_xy_NA(mapping)

  # remap TRUE/FALSE dots
  if(remap_dots) remap_dots_TRUE(mapping)
  if(!remap_dots) remap_dots_FALSE(mapping)



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
    x.eval <- data %>% dplyr::select(eval(x)) %>% names() %>% list()
    is.x <- TRUE
  } else{
    x.eval <- NULL
    is.x   <- FALSE
  }

  #capture y values if exist
  if(hasArg(y)){
    y <- substitute(y)
    y.eval <- data %>% dplyr::select(eval(y)) %>% names() %>% list()
    is.y <- TRUE
  } else{
    y.eval <- NULL
    is.y   <- FALSE
  }

  # capture dots if exist
  dots <- as.list(substitute(list(...)))[-1L]
  if(length(dots) > 0){
    dots.eval <- lapply(seq_along(dots), function(i){
      arg.eval <- data %>% dplyr::select(eval(dots[[i]])) %>% names()
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

