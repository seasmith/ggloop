f <- function(x){
  vect <- c(T, F, NA)
  print(match.arg(as.character(x), as.character(vect)))
}
