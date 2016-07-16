f1 <- function(x, y){
  if(hasArg(x)) x <- substitute(x)
  if(hasArg(y)) y <- substitute(y)

  function(){
    ran.fun <- f2(x, y)
  }
}

f2 <- function(x, y){
  if(missing("x")) print(x)
  if(missing("y")) print(z)
}
