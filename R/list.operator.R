#' Add layers to \code{ggloop} outputs
#'
#' The \code{\%L+\%} (L-plus) operator, is a \code{+} operator wrapped by an \code{lapply()} loop.
#' This function can also be a substitue for adding to "raw" \code{ggplot} objects.
#'
#' \code{\%L+\%} is typically used to add geoms, stats, and other ggplot2 features to
#' the output of \code{ggloop()} (which is often a nested list of \code{ggplot} objects. However
#' it is possible to use \code{\%L+\%} in place of where \code{+} would normally be used. This is
#' due to the conditional statements present in \code{\%L+\%}'s structure.
#'
#' @param lhs Typically a \code{ggloop()} output (a nested list) but can also be a list of \code{ggplot}
#' objects or a single \code{ggplot} object.
#' @param rhs A geom, stat, or other layer feature from the \code{ggplot2} package.
#'
#' @export

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


#' The \code{\%<L>\%} (compound L-plus) operator is  is a mix between magrittr::`%<>%`
#' and ggloop::`%L%`.
#' This operator has the same concept as L-plus, but reassigns the value of the
#' operation back into the original object, much like the compound operator in magrittr.

