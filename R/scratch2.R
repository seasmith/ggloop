outer.fun <- function(arg1, arg2){
  eval(arg2)
  print(arg1)


}

inner.fun <- function(iarg){
  return(iarg)
}

# # Why does the following input exist?
# # Because it is a promise?
# # Both eval(x) and get0(x) throw an error.
# # eval(x) would throw an error if non-standard evaluation
# # were used (similar to dplyr's mpg:hp column naming syntax).
# f3 <- function(x){
#   f4(x)
# }
#
# f4 <- function(x){
#
#   exists <- exists("x")
#   not.missing <- !missing("x")
#   is.promise <- pryr::is_promise(x)
#   try <- tryCatch({
#     get0("x")
#     TRUE
#   }, error = function(e){
#     FALSE
#   })
#
#   print(exists)
#   print(not.missing)
#   print(is.promise)
#   print(try)
# }
#
