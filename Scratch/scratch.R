fun1 <- function(arg1,
                 arg2){
  arg1 <<- arg1
 fun2()
}

fun2 <- function(){
  # e <- parent.env(environment())
  # print(e)
  # arg1 <- get("arg1", envir = e)
  print(paste("The first argument is", arg1))
}

fun3 <- function(){

}






















xy.m <- matrix(unlist(xy), ncol = length(xy))
.nlst.lst <- lapply(seq_len(summary(xy)[1]), function(x){
  c(xy.m[x, ], nlst)
})




# extract first element
sapply(nlst.lst[[1]], `[[`, 1)

# do.call() attempt
extractor <- function(lst, num){
  sapply(lst, `[[`, num)
}

extractor2 <- function(lst, num){
  lapply(lst, function(x){
    sapply(x, function(y){

    })
  })
}

extractor3 <- function(lst, num){
  rslt <- list()
  for(i in seq_len(num)){
  rslt[[i]] <- sapply(lst, `[[`, i)
  }
  return(rslt)
}

do.call(extractor,
        list(lst = nlst.lst[[1]], 1:5))


# Former set_aes() functions ----------------------------------------------

# mcall2 <- function(...)
# {
#   structure(as.list(match.call()[-1]), class = "uneval")
# }

# mcall3 <- function(...)
# {
#   lst <- list(...)
#   structure(lst, class = "uneval")
# }

# Other -------------------------------------------------------------------


# if(lst$is.dots.chained){
#   start <- which(names(lst) %in% "is.dots") + 1 # start of first dots argument
#   end <- length(lst) - 1  # subtract for $is.dots.chained
#
#   lapply(seq_along(start:end), function(){
#     Map(f = )
#   })
# } else{
#
# }
