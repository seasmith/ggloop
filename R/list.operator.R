
# `%L%`() -----------------------------------------------------------------
#
#' Add layers to \code{ggloop} outputs
#'
#' The \code{\%L+\%} (L-plus) operator, is a \code{+} operator wrapped by an
#' \code{lapply()} loop. This function can also be a substitue for adding to
#' "raw" \code{ggplot} objects.
#'
#' \code{\%L+\%} is a substitute for \code{+} and is used in the same fashion:
#' to add geoms, stats, aesthetics, facets, and other features to \code{ggplot}
#' object. The returned object from \code{ggloop()} is often a nested list of
#' \code{ggplot} objects. However it is possible to use \code{\%L+\%} in place
#' of where \code{+} would normally be used. This is due to the conditional
#' statements present in \code{\%L+\%}'s structure.
#'
#' @param lhs Typically the returned object by \code{ggloop()}: either a nested
#'   list of \code{ggplot} objects or a list of \code{ggplot} object, but can
#'   also be a single \code{ggplot} object.
#' @param rhs A geom, stat, or other layer feature from the \code{ggplot2}
#'   package.
#'
#' @export

`%L+%` <- function(lhs, rhs){

  # 1. lhs = ggplot
  test_ggplot <- ggplot2::is.ggplot(lhs)

  # 2. lhs = list()
  test_list <- is.list(lhs)

  # 3. lhs = list(ggplot)
  test_list.ggplot <- tryCatch({
    all(vapply(lhs, ggplot2::is.ggplot, logical(1)))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  # 4. lhs = list(list())
  test_list.list <- tryCatch({
    all(vapply(lhs, is.list, logical(1)))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  # 5. lhs = list(list(ggplot))
  test_list.list.ggplot <- tryCatch({
    all(sapply(lhs, function(x){
      vapply(x, ggplot2::is.ggplot, logical(1))
    }))
  }, warning = function(w){
    FALSE
  }, error = function(e){
    FALSE
  })

  # Create logical vector indicating which scenario - if any
  lhs.test <- c(first = test_ggplot,
                second = all(test_list, test_list.ggplot),
                third = all(test_list.list, test_list.list.ggplot))

  # Begin listing functions for each of three scenarios
  # First scenario
  first <- function() lhs + rhs

  # Second scenario
  second <- function(){
    lapply(lhs, function(x){
      x + rhs
    })
  }

  # Third scenario
  third <- function(){
    lapply(lhs, function(x){
      lapply(x, function(y){
        y + rhs
      })
    })
  }

  # Create list of scenario functions
  fun.list <- list(first,
                   second,
                   third)

  if(sum(lhs.test)){
    fun.list[[which(lhs.test)]]()
  } else{
    stop("Left-hand side is not a ggplot object, list of ggplot objects,
         or a nested list of ggplot objects")
  }

}


# `%<L>%`() ---------------------------------------------------------------
#' The \code{\%<L>\%} (compound L-plus) operator is  is a mix between
#' magrittr::`%<>%` and ggloop::`%L%`. This operator has the same concept as
#' L-plus, but reassigns the value of the operation back into the original
#' object, much like the compound operator in magrittr.

