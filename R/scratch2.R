
eval.c <- function(x){
  if(identical(deparse(x[[1L]]), "c")) x <- x[-1L]
  x
}


x.eval[kp] <- lapply(kp, function(i){
  messy_eval(xy$u[c(1, i)], vars, names_list)
})


# messy_eval --------------------------------------------------------------
#
#' Reduce the amount of code by turning this sequence into a function.

messy_eval <- function(i, vars, names_list){
  lazyeval::lazy_dots(eval(i)) %>%
    lazyeval::as.lazy_dots() %>%
    lazyeval::lazy_eval(c(names_list, select_helpers)) %>%
    magrittr::extract2(1L) %>% vars[.]
}


# rm.gg2() ----------------------------------------------------------------
#
#' Remove \code{ggplot2} style and stand-alone aesthetic arguments (i.e.
#' \code{y}, \code{x:z}, etc).

rm.gg2 <- function(x){
  # x may be a list; fine for is.fun() but not good for is.ops()
  if(is.list(x)) ops <- is.op(x[[1L]])
  else ops <- is.op(x)

  funs <- is.fun(x)
  -c(ops, funs)
}
