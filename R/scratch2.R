f1 <- function(arg2, arg1 = f2()){
  assign("arg2", arg2, envir = environment())
  arg1
}

f2 <- function(){
  pf <- parent.frame()
  print("arg2 is:")
  print(pf$arg2)
}
