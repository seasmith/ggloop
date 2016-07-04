
# isFALSE() ---------------------------------------------------------------
#
#' This is an abbreviation of identical(FALSE, x) to go along with isTRUE()
#'
#' Use this when needing to test explicitly if a value is FALSE.

isFALSE <- function(x) identical(FALSE, x)


# list.pos() --------------------------------------------------------------
#
#' Finds the position of a named list element within a list (with no
#' recurssion).
#'
#' All elements in the input list must have a name for this function to give
#' accurate positions. This function can accept a character vector and return
#' the position of each name in the vector.
#'
#' Will return a character vector with names for each element corresponding
#' to the names in the character vector given to the function.If a name is not
#' present in the list then NA is returned.
#'
#' @param  name A character vector. Ideally a character vector of length 1
#' (just one name); hoever it can accept a character vector of length greater
#' than 1. The names in the character vector will be used as names (element
#' headings) in the results vector.
#' @param lst A list with all elements named. If each element does not have a
#' name then there can be no garuantee to the accuracy of the results.

list.pos <- function(name, lst){
  matches <- sapply(name, function(x){
    matched <- which(names(lst) %in% x)
    if(length(matched) == 0) matched <- NA
    matched
  })
  return(matches)
}

# extract() ---------------------------------------------------------------


extract <- function(lst, num){
  extracted <- list()
  for(i in seq_len(num)){
    extracted[[i]] <- sapply(lst, `[[`, i)
  }
  return(extracted)
}
