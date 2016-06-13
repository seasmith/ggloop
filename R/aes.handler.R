# lapply(seq_along(1:x), function(x, y){
#   to.delete <- (1:y)[-x]
# }, y = x)


# Basic mapply() ----------------------------------------------------------

aes_inputs <- function(data, x.vars, y.vars, ...)
  mapply(FUN = ggplot2::aes,
               x   = x.vars,
               y   = y.vars,
               ... = ...vars,
               SIMPLIFY = F)
