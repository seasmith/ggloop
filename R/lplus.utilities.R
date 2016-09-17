# split_chain() -----------------------------------------------------------
split_chain <- function(expr)
{
  # lists for holding the right-hand sides and the pipe operators.
  rhss  <- list()
  pipes <- list()

  # Process the call, splitting it at each valid %L+% operator.
  i <- 1L
  while(is.call(expr) && is_L(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]
    rhss[[i]] <- rhs

    expr <- expr[[2L]]
    i <- i + 1L
  }

  # return the components; expr will now hold the left-most left-hand side.
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}


# is_L() ------------------------------------------------------------------
is_L <- function(spec_op) identical(spec_op, quote(`%L+%`))


# eval_lhs() --------------------------------------------------------------
eval_lhs <- function(lhs) {
  # lhs = ggplot
  test_ggplot <- ggplot2::is.ggplot(lhs)

  # lhs = list()
  test_list <-
    if (test_ggplot)
      FALSE
  else
    is.list(lhs)

  # lhs = list(ggplot)
  test_list.ggplot <-
    if (test_list)
      all(vapply(lhs, ggplot2::is.ggplot, logical(1)))
  else
    FALSE

  # lhs = list(list())
  test_list.list <-
    if (test_list && !test_list.ggplot)
      all(vapply(lhs, is.list, logical(1)))
  else
    FALSE

  # lhs = list(list(ggplot))
  test_list.list.ggplot <-
    if (test_list.list)
      all(sapply(lhs, function(x) vapply(x, ggplot2::is.ggplot, logical(1))))
  else
    FALSE

  # if (any(is.na(test_list.list, test_list.list.ggplot)))
  #   stop("Left-hand side is not a list of a list of ggplot objects")

  # Create logical vector indicating which scenario - if any
  c(first = test_ggplot,
    second = all(test_list, test_list.ggplot),
    third = all(test_list.list, test_list.list.ggplot))
}

# flist() -----------------------------------------------------------------
flist <- list(
  # Single ggplot object.
  first = function(lhs, rhs) lhs + rhs,

  # List of ggplot objects.
  second = function(lhs, rhs) {
    lapply(lhs, function(x) {
      x + rhs
    })
  },

  # Nested list (list of list) of ggplot objects.
  third = function(lhs, rhs) {
    lapply(lhs, function(x) {
      lapply(x, function(y) {
        y + rhs
      })
    })
  }

)


# nest_function() ---------------------------------------------------------
nest_function <- function(fun, chain) {
  # Set starter variable and iterator.
  nest.this <- chain$lhs
  i <- 1L

  while(i <= length(chain$rhss)) {
    nest.this <- call(fun, nest.this, chain$rhss[[i]])
    i <- i + 1L
  }

  nest.this
}
