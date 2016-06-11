

# gglist() ----------------------------------------------------------------


gglist <- function(data,
                   aes = aes_list(),
                   recycle = TRUE){

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

      dots <- substitute(list(...))[-1]
    dot.vars <- lapply(seq_along(dots), function(i){
      arg.vars <- data %>% dplyr::select(eval(dots[[i]])) %>% names()
    }) %>%
      set_names(names(dots))

  aes_inputs <- c(x = x.vars, y = y.vars, dot.vars)
}



# recycler() --------------------------------------------------------------


recycler <- function(lst){
  logic <- c("x", "y") %in% names(lst)
  if(all(logic)){
    larger <- which.max(length(lst[["x"]]), length(lst[["y"]]))

  } else if(any(logic)){

  }
}
