

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
    dot.eval <- lapply(seq_along(dots), function(i){
      arg.eval <- data %>% dplyr::select(eval(dots[[i]])) %>% names()
      }) %>%
    magrittr::set_names(names(dots))
  is.dot <- TRUE
  } else{
    dot.eval <- NULL
    is.dot   <- FALSE
  }

  # list logical existance and values (if any) for all arguments
  mappings <- c(is.x = is.x, x = x.eval,
                is.y = is.y, y = y.eval,
                is.dot = is.dot, dot.eval)
  return(mappings)
}


# remap_xy_TRUE() -----------------------------------------------------------

remap_xy_TRUE <- function(lst){
  if(lst$is.x && lst$is.y){
    xy <- expand.grid(x = lst$x, y = lst$y, stringsAsFactors = F)

    is.dupes <- mapply(FUN = c, xy$x, xy$y, SIMPLIFY = F) %>%
      lapply(sort) %>%
      duplicated()
    dupes <- if(sum(is.dupes)) which(dupes) else NULL

    is.dubs <- which(xy$x == xy$y)
    dubs <- if(sum(is.dubs)) which(dubs) else NULL

    deletes <- c(dupes, dubs)
    if(!is.null(deletes)) xy <- xy[-deletes, ]

    lst$x <- xy$x
    lst$y <- xy$y
  }
  return(lst)
}


# remap_xy_FALSE() ----------------------------------------------------------


remap_xy_FALSE <- function(lst){
  if(is.x && is.y){
    lengths <- c(length(lst[["x"]]), length(lst[["y"]]))
    xy.max <- whichxy.max(lengths)
    xy.min <- whichxy.min(lengths)
    xy.max.length <- length(lst[[xy.max]])

    lst[[xy.min]] <- lst[[xy.min]][1L:xy.max.length]
  }
  return(lst)
}


# remap_xy_NA() ---------------------------------------------------------


remap_xy_NA <- function(lst){
  if(is.x && is.y){
    lengths <- c(length(lst[["x"]]), length(lst[["y"]]))
    .max <- which.max(lengths)
    .min <- which.min(lengths)

    quotient <- length(lst[[.max]]) %/% length(lst[[.min]])
    remainder <- length(lst[[.max]]) %% length(lst[[.min]])
    if.zero <- !is.na(remainder/remainder) %>% sum() # 0 if 0/0; 1 if !0/0

    .min.quotient <- rep(lst[[.min]], quotient)
    .min.remainder <- rep(lst[[.min]][1L:remainder], if.zero) # rep 0 or 1

    lst[[.min]] <- c(.min.quotient, .min.remainder)
  }
  return(lst)
}


# remap_dots_TRUE() ---------------------------------------------------------


remap_dots_TRUE <- function(lst){
  if(is.dots){
    combo <- expand.grid(lst[-(1:2)], stringsAsFactors = F)
    lst[3:length(lst)] <- combo[1:length(combo)]
  }
  return(lst)
}


# remap_dots_FALSE() --------------------------------------------------------


remap_dots_FALSE <- function(lst){
  if(is.dots){
    dots <- lst[3:length(lst)]
    no.recycle <- sapply(dots, length) %>%
      which.max() %>%
      lapply(dots[-.], function(x, y) x[1L:length(dots[[y]])], y = .)
    lst[names(no.recycle)] <- no.recycle
    }
  return(lst)
  }


