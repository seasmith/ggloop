
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
