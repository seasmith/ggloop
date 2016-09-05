gg <- function(mapping, ...){
  mapping.FUNs <- parse(text = mapping)
  mapping.args <- formals(mapping)
  mapping <- substitute(mapping)
  dots <- as.list(substitute(list(...)))[-1L]
  list(mapping = mapping, mapping.args = mapping.args,
       mapping.FUNs, dots = dots)
}

y <- function(){

}

x <- function(){

}

aes_inside <- function(x, y, ...){
  x <- substitut(x)
  y <- substitute(y)
  dots <- as.list(substitute(list(...)))[-1L]
    function(i){

    i$mapping
  }
}

`%LL%` <- function(lhs, rhs){
  lhs.substitute <- substitute(lhs)
  lhs.parsed <- parse(text = lhs.substitute)

  rhs.substitute <- substitute(rhs)
  rhs.parsed <- parse(text = rhs.substitute)

  list(lhs.substitute = lhs.substitute,
       lhs.parsed = lhs.parsed,
       rhs.substitute = rhs.substitute,
       rhs.parsed = rhs.parsed)
}
