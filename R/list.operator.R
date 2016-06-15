
# %L+%() ------------------------------------------------------------------


# The %L+% (L-plus) operator, is a + operator wrapped by an lapply() loop.
# The left hand argument is evaluated for two truths:
#   is.list
#   is.ggplot
# If both these tests are TRUE then the right-hand argument is passed as
# an additional argument in the lapply() loop. The lapply() loop is iterated
# on seq_along() the left-hand argument (the list of ggplot objects)


`%L+%` <- function(lhs, rhs){
  logic.1 <- is.list(lhs)
  logic.2 <- all(vapply(lhs, is.ggplot, FUN.VALUE = logical(1)))

  if(logic.1 & logic.2)
    lapply(seq_along(lhs), function(x, y){
      lhs[[x]] + y
    }, y = rhs)
}
