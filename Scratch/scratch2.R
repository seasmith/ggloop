
eval.c <- function(x){
  if(identical(deparse(x[[1L]]), "c")) x <- x[-1L]
  x
}


x.eval[kp] <- lapply(kp, function(i){
  messy_eval(xy$u[c(1, i)], vars, names_list)
})


# Step 1: Make dots -------------------------------------------------------


dts <- mkdots(color = c(factor(gear), gear + cyl, mpg:cyl),
              lty = mpg:cyl,
              fill = factor(gear))

# Step 2: Strip dots of c() -----------------------------------------------

dots.names <- names(dts)

dts <- lapply(seq_along(dts), function(x){
  if(is.c(dts[[x]][[1L]])) dts[[x]][-1L]
  else list(dts[[x]])
})


# Step 3: Remove and Keep -------------------------------------------------

rm <- sapply(dts, function(x){
  rm.gg2(x) %||% FALSE
})

kp <- lapply(seq_along(dts), function(i){
  if(isFALSE(rm[[i]])){
    seq_along(dts[[i]])
    } else{
      seq_along(dts[[i]])[rm[[i]]] %||% FALSE
    }
})



# Step 4: Evaluate --------------------------------------------------------


dots.eval <- list()

# Evaluate ggplot2-like expressions.
kp.eval <- lapply(seq_along(kp), function(i){
  if(isFALSE(kp[[i]])) NULL
  else {
    d.eval <- list()
    d.eval[kp[[i]]] <- lapply(kp[[i]], function(j) messy_eval(dts[[i]][[j]], vars, names_list))
    d.eval
  }
})

# Evaluate dplyr-like expressions.
rm.eval <- lapply(seq_along(dts), function(i){
  d.eval <- list()
  d.eval[rev(abs(rm[[i]]))] <- sapply(dts[[i]][rev(abs(rm[[i]]))], deparse) %||% NULL
})



# Step 5: Combine Evaluations ---------------------------------------------

dots.eval <- lapply(seq_along(dts), function(x) c(unlist(rm.eval[[x]]), unlist(kp.eval[[x]])))
names(dots.eval) <- dots.names
# lapply(seq_along(kp), function(i){
#   if(is.c(dts[[i]][[1L]])) x <- dts[[i]][-1L]
#   else x <- list(dts[[i]])
#   sapply(kp[i], function(j){
#     messy_eval(x[[j]], vars, names_list)
#   })
# })

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
