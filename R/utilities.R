#' @importFrom magrittr %>%
NULL

# isFALSE() ---------------------------------------------------------------
#
#' @title
#' This is an abbreviation of identical(FALSE, x) to go along with isTRUE()
#'
#' @description
#' Use this when needing to test explicitly if a value is FALSE.
#'
#' @param x An object to be tested.

isFALSE <- function(x) identical(FALSE, x)


# `%R%`() -----------------------------------------------------------------
#
#' The replacement operator. Replaces the \code{lhs} with \code{rhs} on the
#' condition that \code{length(lhs) == FALSE} (the length is \code{0}).
#'
#' @param lhs An object of any length.
#' @param rhs A replacement value if  \code{length(lhs) == FALSE}.

`%R%` <- function(lhs, rhs){
  if(length(lhs)) lhs else rhs
}


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
#' @param  x A character vector of possible list element names. The names in the
#'   character vector will be used as names (element headings) in the results
#'   vector.
#' @param lst A list with all elements named. If each element does not have a
#' name then there can be no garuantee to the accuracy of the results.

list.pos <- function(x, lst) {

  vars <- names(lst)

  matches <- sapply(x, function(i) {

    matched <- which(vars %in% i)
    if (!length(matched)) matched <- NA
    matched

  })

  return(matches)

}


# extract() ---------------------------------------------------------------
#
#' Extract the nth element from vectors in a list.
#'
#' \code{extract()} simply uses a \code{for} loop to extract the nth element
#' from each vector in a list. However, it can also operate on a data frame.
#' This is equivalent to taking the first element of each vector and making a
#' those elements the first vector in a new list, and it continues on so until
#' it reaches the last element.
#'
#' @param lst A list of vectors of equal length, a data frame, or a matrix.
#' If the length of the smallest vector in \code{lst} is smaller than
#' \code{num} then an error will be thrown (\code{subscript out of bounds}).
#' @param num A number (preferably the length of the vectors) to create a
#' sequence for \code{extract()} to extract the elements of \code{lst}. Default
#' value is the length of the shortest vector in the list.

extract <- function(lst, num = min(lengths(lst))) {
#   extracted <- list()
#   for(i in seq_len(num)){
#     extracted[[i]] <- sapply(lst, `[[`, i)
#   }
#   return(extracted)

  # classes <- vapply(lst, class, character(1))
  # class.fun <- lapply(classes, match.fun)
  # lst <- lapply(lst, as.character)
  extracted <- lapply(seq_len(num), function(i) {
    sapply(lst, `[[`, i)
    # extracted[[i]] <- vapply(lst, `[[`, cl(1), i)
  })
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
#' @param ... Vectors to be expanded.
#' @param rm.dupes Removes duplicated "rows". If \code{TRUE} (default) then
#' rows that are unordered duplicates of other rows will be removed. i.e.
#' \code{c("A", "B", "C")} is the same as \code{c("C", "B", "A")}
#'  and any other combination of \code{"A"}, \code{"B"}, and \code{"C"}.
#' @param rm.dubs Removes a row in which all elements are the same. If
#' \code{TRUE} (default) then a row such as \code{c("A", "A", "A")} will be
#' removed.

expand.grid2 <- function(..., rm.dupes = TRUE, rm.dubs = TRUE) {

  args  <- list(...)
  nargs <- length(args)

  grid <- expand.grid(args, stringsAsFactors = FALSE)
    grid.names  <- names(grid)
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

  if (rm.dupes) {
    is.dupes <- lapply(grid.list, sort) %>%
      duplicated()
    dupes <- if (sum(is.dupes)) which(is.dupes) else NULL
  } else {
    dupes <- NULL
  }

  if (rm.dubs) {
    is.dubs <- lapply(grid.list, duplicated) %>%
      lapply(sum) %>%
      `==`(nargs - 1)
    dubs <- if (sum(is.dubs)) which(is.dubs) else NULL
  } else {
    dubs <- NULL
  }

  deletes <- c(dupes, dubs)
  if (!is.null(deletes)) grid <- grid[-deletes, ]
  names(grid) <- grid.names

  return(grid)
}


# `%M%`() -----------------------------------------------------------------
#
#' The modified combination of the modulus function (\code{\%\%}) and
#' integer divisor function (\code{\%/\%}).
#'
#' The placement of the arguments (\code{lhs} and \code{rhs}) does not matter
#' unlike the actual modulus function (\code{\%\%}) and integer divisor
#' function (\code{\%/\%})
#'
#' @param lhs A number (integer or numeric)
#' @param rhs A number (integer or numeric)

`%M%` <- function(lhs, rhs) {
  if (lhs < rhs) {
    old.lhs <- lhs
    lhs <- rhs
    rhs <- old.lhs
  }
  x <- lhs %/% rhs
  y <- lhs %% rhs
  return(c(quotient = x, remainder = y))
}


# recycle.NA() ------------------------------------------------------------
#
#' @title
#' A vector recycler using \code{NA}.
#'
#' @description
#' Will recycle using \code{NA} rather than imitating \code{R}'s internal
#' recycling mechanism.
#'
#' @param x,y Vectors, of which the shorter will be recycled.

recycle.NA <- function(x, y) {
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)
  xy.max.length <- length(xy.list[[xy.max]])

  xy.list[[xy.min]] <- xy.list[[xy.min]][1L:xy.max.length]
  return(xy.list)
}


# recycle.vector() --------------------------------------------------------
#
#' @title
#' A vector recycler using the contents of the shorter vector to do the
#' recycling.
#'
#' @description
#' The shorter of the two vectors will be recycled. Imitates \code{R}'s internal
#' recycling mechanism.
#'
#' @param x,y Vectors, of which the shorter will be recycled.

recycle.vector <- function(x, y) {
  if (is.list(x) && length(x) == 2) {
    y <- x[[2]]
    x <- x[[1]]
  }
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)

  if (xy.max == xy.min) return(xy.list)

  division <- xy.lengths[[xy.min]] %M% xy.lengths[[xy.max]]
    if.zero <- !is.na(division[["remainder"]]/division[["remainder"]]) %>% sum()

  xy.quotient <- rep(xy.list[[xy.min]], division[["quotient"]])
  xy.remainder <- rep(xy.list[[xy.min]][1L:division[["remainder"]]], if.zero)

  xy.list[[xy.min]] <- c(xy.quotient, xy.remainder)

  return(xy.list)
}


# what() ------------------------------------------------------------------
#
#' Console function for determing: class, type, mode, and names of an object.
#'
#' @param x An object.
#' @param SIMPLIFY Option to simplify result to a
#' vector (deafult is \code{TRUE}). Result is a list if \code{FALSE}.


what <- function(x, SIMPLIFY = TRUE) {
  if(SIMPLIFY) {
    c(class = class(x),
      type = typeof(x),
      mode = mode(x),
      names = names(x))
  } else {
    list(class = class(x),
         type = typeof(x),
         mode = mode(x),
         names = names(x))
  }
}
