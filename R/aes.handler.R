
# Basic mapply() ----------------------------------------------------------

aes.list <- mapply(FUN = ggplot2::aes,
                   x   = x.vars,
                   y   = y.vars,
                   ... = ...vars,
                   SIMPLIFY = F)
