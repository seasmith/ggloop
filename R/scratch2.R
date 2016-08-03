
is.subsettable <- function(x){

}


 if(x.exists){
   if(is.cwrap(x[[1L]])){

   } else{
     rm <- (rm.gg2(x[-1L]) - 1)
     kp <- if(length(rm) > 0){
       seq_along(x)[rm][-1L]
     } else{
       seq_along(x)[-1L]
     }
   }


   x.eval <- list()
   x.eval[kp - 1] <- lapply(kp, function(i){
     messy_eval(x[c(1, i)], vars, names_list)
   })
   if(length(rm) > 0) x.eval[-(rm + 1)] <- sapply(x[-rm], deparse)

   x.eval <- unlist(x.eval) %>% `names<-`(NULL)
 } else{
   x.eval <- NULL
 }

# vars <- names(mtcars)
exp <- substitute(c(mpg:cyl, mpg/cyl, mpg + cyl^2, as.factor(cyl), 1:3))
# exp2 <- as.list(substitute(list(colour = as.factor(cyl), shape = mpg:hp)))[-1L]
rm.gg2 <- function(x){
  arths <- is.arth(x)
  funs <- is.fun(x)
  -c(arths, funs)
}
#
rm <- (rm.gg2(exp[-1L]) - 1)  # take these out
kp <- seq_along(exp)[rm][-1L] # select these
to_lazy <- exp[c(1, c(kp))]
#
# new.exp <- list()
# new.exp[kp - 1] <- lapply(kp, function(x) messy_eval(exp[c(1,x)], vars))
# new.exp[-(rm + 1)] <- sapply(exp[-rm], deparse)
#
# first.eval <- messy_eval(exp.to_lazy, vars)
#
# # aes_eval ----------------------------------------------------------------
# #
# if(x.exists){
#   rm <- (rm.gg2(x[-1L]) - 1)
#   kp <- seq_along(x)[rm][-1L]
#   x.eval <- list()
#   x.eval[kp - 1] <- sapply(kp, function(i) messy_eval(x[c(1, i)], vars))
#   x.eval[-(rm + 1)] <- sapply(x[-rm], deparse)
# } else{
#   x.eval <- NULL
# }
#
# if(y.exists){
#   rm <- (rm.gg2(y[-1L]) - 1)
#   kp <- seq_along(y)[rm][-1L]
#   y.eval <- list()
#   y.eval[kp - 1] <- sapply(kp, function(i) messy_eval(y[c(1, i)], vars))
#   y.eval[-(rm + 1)] <- sapply(y[-rm], deparse)
# } else{
#   y.eval <- NULL
# }
#
# if(length(dots) > 0){
#   rm <- rm.gg2(dots)
#   kp <- seq_along(dots)[rm]
#   dots.eval <- list()
#   dots.eval[-rm] <- sapply(dots[-rm], deparse)
#   dots.eval[kp] <- lapply(seq_along(dots[rm]), function(i) {
#     messy_eval(dots[rm][[i]], vars)
#     })
#   names(dots.eval) <- names(dots)
#   is.dots <- TRUE
# } else{
#   dots.eval <- NULL
#   is.dots   <- FALSE
# }
