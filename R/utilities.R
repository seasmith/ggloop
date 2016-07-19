
# magrittr::`%>%` ---------------------------------------------------------
#' This is an alias for \code{magrittr::`\%>\%`}.

`%>%` <- magrittr::`%>%`


# magrittr::`%<>%` --------------------------------------------------------
#' This is an alias for \code{magrittr::`\%<>\%`}.

`%<>%` <- magrittr::`%<>%`


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

    # logic <- names(lst) %>% sapply(function(x) !is.na(x) && nzchar(x))
    # chars <- which(logic)
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



# expand.grid2() ----------------------------------------------------------
#
#' A new version of an old favorite with some extra options
#'
#' \code{expand.grid2()} creates a combination data frame from vectors or lists
#' but differs from the original \code{expand.grid()} in that it has two options
#' for removing two different type of duplicates. \code{stringsAsFactors} is
#' set to \code{TRUE}.
#'
#' @param rm.dupes Removes duplicated "rows". If \code{TRUE} (default) then
#' rows that are unordered duplicates of other rows will be removed. i.e.
#' \code{c("A", "B", "C")} is the same as \code{c("C", "B", "A")}
#'  and any other combination of \code{"A"}, \code{"B"}, and \code{"C"}.
#' @param rm.dubs Removes a row in which all elements are the same. If
#' \code{TRUE} (default) then a row such as \code{c("A", "A", "A")} will be
#' removed.

expand.grid2 <- function(..., rm.dupes = TRUE, rm.dubs = TRUE){
  args <- list(...)
  nargs <- length(args)

  grid <- expand.grid(args, stringsAsFactors = FALSE)
    grid.names <- names(grid)
    names(grid) <- NULL

  grid.list <- extract(grid, nrow(grid))

  # if(rm.dupes){
  #   is.dupes <- lapply(seq_along(grid.list), function(x){
  #     names(grid.list[[x]]) <- NULL
  #     grid.list[[x]]}) %>%
  #     lapply(sort) %>%
  #     duplicated()
  #   dupes <- if(sum(is.dupes)) which(is.dupes) else NULL
  # } else{
  #   dupes <- NULL
  # }

  if(rm.dupes){
    is.dupes <- lapply(grid.list, sort) %>%
      duplicated()
    dupes <- if(sum(is.dupes)) which(is.dupes) else NULL
  } else{
    dupes <- NULL
  }

  if(rm.dubs){
    is.dubs <- lapply(grid.list, duplicated) %>%
      lapply(sum) %>%
      `==`(nargs - 1)
    dubs <- if(sum(is.dubs)) which(is.dubs) else NULL
  } else{
    dubs <- NULL
  }

  deletes <- c(dupes, dubs)
  if(!is.null(deletes)) grid <- grid[-deletes, ]
  names(grid) <- grid.names

  return(grid)
}



# time.test() -------------------------------------------------------------
#
#' A simple test for evaluation speed.
#'
#' Tests the time it takes to evaluate an expression (such as a function). Uses
#' \code{proc.time()} to keep track of all three different time types:
#' \code{user}, \code{system}, and \code{elapsed}.
#'
#' @param x An expression, such as a function, arithmetic equation, etc.

time.test <- function(x){
  p1 <- proc.time()
  eval(x)
  p2 <- proc.time() - p1
  print(p2)
}
