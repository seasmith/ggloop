xy <- nputs[1:2]
dots <- nputs[3:length(nputs)]
.dots[rep(1, length(nputs[[1]])), ]


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


