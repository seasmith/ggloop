

# Basic mapply() ----------------------------------------------------------

aes_inputs <- function(data, x.vars, y.vars, ...)
  mapply(FUN = ggplot2::aes,
               x   = x.vars,
               y   = y.vars,
               ... = ...vars,
               SIMPLIFY = F)
