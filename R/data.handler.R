

# ggloop() ----------------------------------------------------------------


ggloop <- function(data,
                   mapping = aes_list(),
                   recycle = TRUE,
                   environment = parent.frame()){

}


# aes_list() --------------------------------------------------------------


aes_list <- function(x,
                     y,
                     ...){
  mapply(FUN = ggplot2::aes,
         x = x,
         y = y,
         ... = ...,
         SIMPLIFY = F)
}


# aes_inputs() ------------------------------------------------------------


aes_inputs <- function(data,
                     x,
                     y,
                     ...){
  if(hasArg(x)){
    x <- substitute(x)
    x.vars <- data %>% dplyr::select(eval(x)) %>% names() %>% list()
  } else{
    x.vars <- NULL
  }

  if(hasArg(y)){
    y <- substitute(y)
    y.vars <- data %>% dplyr::select(eval(y)) %>% names() %>% list()
  } else{
    y.vars <- NULL
  }

      dots <- as.list(substitute(list(...)))[-1L]
    dot.vars <- lapply(seq_along(dots), function(i){
      arg.vars <- data %>% dplyr::select(eval(dots[[i]])) %>% names()
    }) %>%
      magrittr::set_names(names(dots))

  aes_inputs <- c(x = x.vars, y = y.vars, dot.vars)
}



# recycle_TRUE() ----------------------------------------------------------


recycle_TRUE <- function(lst){
  logic <- c("x", "y") %in% names(lst)
  if(all(logic)){
    lengths <- c(length(lst[["x"]]), length(lst[["y"]]))
    .max <- which.max(lengths)
    .min <- which.min(lengths)

    quotient <- length(lst[[.max]]) %/% length(lst[[.min]])
    remainder <- length(lst[[.max]]) %% length(lst[[.min]])
      if.zero <- !is.na(remainder/remainder) %>% sum() # 0 if 0/0; 1 if !0/0
    .min.quotient <- rep(tst[[.min]], quotient)
    .min.remainder <- rep(tst[[.min]][1L:remainder], if.zero) # rep 0 or 1

    lst[[.min]] <- c(.min.quotient, .min.remainder)
  } #else if(logic[[1L]]){

  # } else if(logic[[2L]]){
  #   lst[["x"]] <- lst[["y"]]
  #   lst[["y"]] <- NULL
  # }
  return(lst)
}



# recycle_FALSE() ---------------------------------------------------------


recycle_FALSE <- function(lst){
  logic <- c("x", "y") %in% names(lst)
  if(all(logic)){
    lengths <- c(length(lst[["x"]]), length(lst[["y"]]))
    .max <- which.max(lengths)
    .min <- which.min(lengths)
    .max.length <- length(lst[[.max]])

    lst[[.min]] <- tst[[.min]][1L:.max.length]
  } else if(logic[[1L]]){

  } else if(logic[[2L]]){
    lst[["x"]] <- lst[["y"]]
    lst[["y"]] <- NULL
  }
  return(lst)
}
