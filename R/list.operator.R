
# %L+%() ------------------------------------------------------------------


# The %L+% (L-plus) operator, is a + operator wrapped by an lapply() loop.
# The left hand argument is evaluated for two truths:
#   is.list
#   is.ggplot
# If both these tests are TRUE then the right-hand argument is passed as
# an additional argument in the lapply() loop. The lapply() loop is iterated
# on seq_along() the left-hand argument (the list of ggplot objects)


# `%L+%` <- function(lhs, rhs){
#   logic.1 <- is.list(lhs)
#   logic.2 <- all(vapply(lhs, ggplot2::is.ggplot, FUN.VALUE = logical(1)))
#   logic.3 <- ggplot2::is.ggproto(rhs)
#
#   if(logic.1 && logic.2 && logic.3)
#     lapply(seq_along(lhs), function(x, y){
#       lhs[[x]] + y
#     }, y = rhs)
# }

# This needs some modification because the lhs will be
# a list of lists of lists. This will require a second lapply() for
# evaluation and adding the rhs into the lhs.

`%L+%` <- function(lhs, rhs){
  if(!ggplot2::is.ggproto(rhs)) stop("The rhs must be of class ggproto")

  if(!is.list(lhs)){
    if(!ggplot2::is.ggplot(lhs)) stop("The lhs has no ggplot object")
  }

  # if lhs is.list = TRUE AND is.ggplot = TRUE
  if(ggplot2::is.ggplot(lhs)){
    lhs + rhs
    } else{

    is.all.list <- all(vapply(lhs,
                              is.list,
                              FUN.VALUE = logical(1)))
    # is.all.ggplot <- all(vapply(lhs,
    #                             ggplot2::is.ggplot,
    #                             FUN.VALUE = logical(1)))
    if(!is.all.list && !ggplot2::is.ggplot(lhs)){
      is.all.ggplot <- all(vapply(lhs,
                                  ggplot2::is.ggplot,
                                  FUN.VALUE = logical(1))) #logic.1.2
      if(!is.all.ggplot) stop("The lhs list is not all ggplot objects")
        lapply(seq_along(lhs), function(x, y){
                lhs[[x]] + y
              }, y = rhs)
    }

  is.all.ggplot <- all(sapply(lhs, function(x){
    vapply(x, ggplot2::is.ggplot, FUN.VALUE = logical(1))
  })
  )
  if(!is.all.ggplot) stop("The list of lists is not in proper format:
                    each object in final list element must be of
                    class ggplot")
  lapply(seq_along(lhs), function(x, z){
    lapply(lhs[[x]], function(y){
      y + z
    })
  }, z = rhs)
    }
}


# %<L>%() -----------------------------------------------------------------

# The compound L-plus operator is  is a mix between magrittr::`%<>%`
# and ggloop::`%L%`.
# This operator has the same concept as L-plus, but reassigns the value of the
# operation back into the original object, much like the compound operator in magrittr.

