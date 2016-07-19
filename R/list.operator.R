
# `%L%`() -----------------------------------------------------------------
#' Add layers to \code{ggloop} outputs
#'
#' The \code{\%L+\%} (L-plus) operator, is a \code{+} operator wrapped by an
#' \code{lapply()} loop. This function can also be a substitue for adding
#' to "raw" \code{ggplot} objects.
#'
#' \code{\%L+\%} is a substitute for \code{+} and is used in the same fashion: to
#' add geoms, stats, aesthetics, facets, and other features to \code{ggplot}
#' object. The returned object from \code{ggloop()} is often a nested list of
#' \code{ggplot} objects. However it is possible to use \code{\%L+\%} in place
#' of where \code{+} would normally be used. This is due to the conditional
#' statements present in \code{\%L+\%}'s structure.
#'
#' @param lhs Typically the returned object by \code{ggloop()}: either a nested
#' list of \code{ggplot} objects or a list of \code{ggplot} object, but can also
#' be a single \code{ggplot} object.
#' @param rhs A geom, stat, or other layer feature from the \code{ggplot2}
#' package.
#'
#' @export

`%L+%` <- function(lhs, rhs){

  # 1. lhs = ggplot
  test_ggplot <- ggplot2::is.ggplot(lhs)

  # 2. lhs = list(ggplot)
  test_list <- is.list(lhs)

  test_list.ggplot <- tryCatch({
    all(sapply(lhs, ggplot2::is.ggplot))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  # 3. lhs = list(list(ggplot))
  test_list.list <- tryCatch({
    all(sapply(lhs, is.list))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  test_list.list.ggplot <- tryCatch({
    all(sapply(lhs, function(x){
      sapply(x, ggplot2::is.ggplot)
    }))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  lhs.test <- c(first = test_ggplot,
                second = all(test_list, test_list.ggplot),
                third = all(test_list.list, test_list.list.ggplot))

  # First case
  first <- function() lhs + rhs

  # Second case
  second <- function(){
    lapply(lhs, function(x){
      x + rhs
    })
  }

  # Third case
  third <- function(){
    lapply(lhs, function(x){
      lapply(x, function(y){
        y + rhs
      })
    })
  }

  fun.list <- list(first,
                   second,
                   third)

  if(sum(lhs.test)){
    fun.list[[which(lhs.test)]]()
  } else{
    stop("Left-hand side is not a ggplot object, list of ggplot objects,
         or a list of lists of ggplot objects")
  }

}


# `%<L>%`() ---------------------------------------------------------------
#' The \code{\%<L>\%} (compound L-plus) operator is  is a mix between magrittr::`%<>%`
#' and ggloop::`%L%`.
#' This operator has the same concept as L-plus, but reassigns the value of the
#' operation back into the original object, much like the compound operator in magrittr.

