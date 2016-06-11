
# Basic mapply() ----------------------------------------------------------

aes.list <- mapply(FUN = aes,
                   x   = x.vars,
                   y   = y.vars,
                   ... = ...vars)



# select ------------------------------------------------------------------

function (.data, ...)
{
  select_(.data, .dots = lazyeval::lazy_dots(...))
}
<environment: namespace:dplyr>


# select_ -----------------------------------------------------------------

function (.data, ..., .dots)
{
  UseMethod("select_")
}
<environment: namespace:dplyr>



# select_vars -------------------------------------------------------------

function (vars, ..., include = character(), exclude = character())
{
  args <- lazyeval::lazy_dots(...)
  select_vars_(vars, args, include = include, exclude = exclude)
}
<environment: namespace:dplyr>
