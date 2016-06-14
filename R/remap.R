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


# remap_xy_NA() -------------------------------------------------------------


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


# remap_xy_FALSE() ----------------------------------------------------------


remap_xy_FALSE <- function(lst){
  if(lst$is.x && lst$is.y){
    xy.lengths <- c(x = length(lst[["x"]]), y = length(lst[["y"]]))
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
    start <- which(names(lst) %in% "is.dots") + 1 # start of first dots argument
    end <- length(lst)
    dots <- expand.grid(lst[start:end], stringsAsFactors = F)
    lst[start:end] <- dots[1:length(dots)]
  }
  return(lst)
}


# remap_dots_FALSE() --------------------------------------------------------


remap_dots_FALSE <- function(lst){
  if(lst$is.dots){
    start <- which(names(lst) %in% "is.dots") + 1 # start of first dots argument
    end <- length(lst)
    dots <- lst[start:end]
    no.recycle <- sapply(dots, length) %>% which.max() %>%
      lapply(dots[-.], function(x, y) x[1L:length(dots[[y]])], y = .)
    lst[names(no.recycle)] <- no.recycle
  }
  return(lst)
}


# exclusive_dots() ----------------------------------------------------------

exclusive_dots <- function(lst){

}
