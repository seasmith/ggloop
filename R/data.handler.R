

# ggloop() ----------------------------------------------------------------


ggloop <- function(data,
                   mapping = aes_loop(),
                   remap_xy = TRUE,
                   remap_dots = TRUE,
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
  if(length(dots)){
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


# remap_xy_TRUE() -----------------------------------------------------------

remap_xy_TRUE <- function(lst){
  if(lst$is.x && lst$is.y){
    xy <- expand.grid(y = lst$y, x = lst$x, stringsAsFactors = F)

    is.dupes <- mapply(FUN = c, xy$x, xy$y, SIMPLIFY = F) %>%
      lapply(sort) %>%
      duplicated()
    dupes <- if(sum(is.dupes)) which(is.dupes) else NULL

    is.dubs <- xy$x == xy$y
    dubs <- if(sum(is.dubs)) which(is.dubs) else NULL

    deletes <- c(dupes, dubs)
    if(!is.null(deletes)) xy <- xy[-deletes, ]

    lst$x <- xy$x
    lst$y <- xy$y
  }
  return(lst)
}


# remap_xy_NA() -------------------------------------------------------


remap_xy_NA <- function(lst){
  if(lst$is.x && lst$is.y){
    xy.lengths <- c(x = length(lst[["x"]]), y = length(lst[["y"]]))
      xy.max <- which.max(xy.lengths) %>% names()
      xy.min <- which.min(xy.lengths) %>% names()
        xy.max.length <- length(lst[[xy.max]])

    lst[[xy.min]] <- lst[[xy.min]][1L:xy.max.length]
  }
  return(lst)
}


# remap_xy_FALSE() ------------------------------------------------------


remap_xy_FALSE <- function(lst){
  if(lst$is.x && lst$is.y){
    xy.lengths <- c(x = length(lst[[2]]), y = length(lst[[4]]))
      xy.max <- which.max(xy.lengths) %>% names()
      xy.min <- which.min(xy.lengths) %>% names()

    quotient <- length(lst[[xy.max]]) %/% length(lst[[xy.min]])
    remainder <- length(lst[[xy.max]]) %% length(lst[[xy.min]])
    if.zero <- !is.na(remainder/remainder) %>% sum() # 0 if 0/0; 1 if !0/0

    xy.quotient <- rep(lst[[xy.min]], quotient)
    xy.remainder <- rep(lst[[xy.min]][1L:remainder], if.zero) # rep 0 or 1

    lst[[xy.min]] <- c(xy.quotient, xy.remainder)
  }
  return(lst)
}


# remap_dots_TRUE() ---------------------------------------------------------


remap_dots_TRUE <- function(lst){
  if(lst$is.dots){
    dots <- expand.grid(lst[-(1:4)], stringsAsFactors = F)
    lst[3:length(lst)] <- dots[1:length(dots)]
  }
  return(lst)
}


# remap_dots_FALSE() --------------------------------------------------------


remap_dots_FALSE <- function(lst){
  if(lst$is.dots){
    dots <- lst[5:length(lst)]
    no.recycle <- sapply(dots, length) %>%
      which.max() %>%
      lapply(dots[-.], function(x, y) x[1L:length(dots[[y]])], y = .)
    lst[names(no.recycle)] <- no.recycle
    }
  return(lst)
  }


