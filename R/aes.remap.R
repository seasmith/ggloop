#' @include utilities.R

# remap_xy_TRUE() -----------------------------------------------------------
#' @title
#' Uses \code{expand.grid()} to create all possible combinations of xy pairings.
#'
#' @description
#' Matching duplicates (xy pairings that contain identical xy values) will be
#' tossed, and unordered duplicate pairs (xy pairings which match another xy
#' pair (i.e. (mpg, cyl) == (cyl, mpg))) will be tossed.
#'
#' @param lst A list. The list passe will be the raw list generated from calling
#' \code{aes_assign()} and is ran before a remap function for the any "dots"
#' in the list.

remap_xy_TRUE <- function(lst) {
  lst <- as.list(lst)
  if (exists("x", lst) && exists("y", lst)) {
    xy <- expand.grid(y = lst$y, x = lst$x, stringsAsFactors = FALSE)

    # Find unordered duplicate pairs and extract them.
    is.dupes <- mapply(FUN = c, xy$x, xy$y, SIMPLIFY = FALSE) %>%
      lapply(sort) %>%
      duplicated()
    dupes <- if (sum(is.dupes)) which(is.dupes) else NULL

    # Find exact duplicates (doubles) and extract them.
    is.dubs <- xy$x == xy$y
        is.dubs[is.na(is.dubs)] <- FALSE
    dubs <- if (sum(is.dubs)) which(is.dubs) else NULL

    # Delete and duplicates or doubles.
    deletes <- c(dupes, dubs)
    if (!is.null(deletes)) xy <- xy[-deletes, ]

    lst$x <- xy$x
    lst$y <- xy$y
  }
  return(lst)
}


# remap_xy_NA() -------------------------------------------------------------
#'
#' @title
#' Attaches NA during recycling of the smaller of the two vectors.
#'
#' @description
#' The smallest of the two vectors (\code{x} or \code{y}) will be recycled with
#' NA instead of using the vector itself (similar to R's internal recycling
#' mechanism).
#'
#' @param lst A list. The list passe will be the raw list generated from calling
#' \code{aes_assign()} and is ran before a remap function for the any "dots"
#' in the list.

remap_xy_NA <- function(lst) {
  lst <- as.list(lst)
  if (exists("x", lst) && exists("y", lst)) {
    # SHOULD I ADD SOMETHING TO DEAL WITH DUPES???
      # what did i mean by that?
    xy.lengths <- c(x = length(lst$x), y = length(lst$y))
    xy.max <- which.max(xy.lengths) %>% names()
    xy.min <- which.min(xy.lengths) %>% names()
    xy.max.length <- length(lst[[xy.max]])

    lst[[xy.min]] <- lst[[xy.min]][1L:xy.max.length]
  }
  return(lst)
}


# remap_xy_FALSE() ----------------------------------------------------------
#'
#' @title
#' Mimicks R's internal recycling mechanism for the shorter of the two vectors.
#'
#' @description
#' The smallest of the two vectors (\code{x} or \code{y}) will be recycled in
#' a manner similar to R's internal recycling mechanism.
#'
#' @param lst A list. The list passe will be the raw list generated from calling
#' \code{aes_assign()} and is ran before a remap function for the any "dots"
#' in the list.

remap_xy_FALSE <- function(lst) {
  lst <- as.list(lst)

  if (exists("x", lst) && exists("y", lst)){
    xy.lengths <- c(x = length(lst$x), y = length(lst$y))
    xy.max     <- which.max(xy.lengths) %>% names()
    xy.min     <- which.min(xy.lengths) %>% names()
      if (xy.max == xy.min) return(lst)

    quotient  <- length(lst[[xy.max]]) %/% length(lst[[xy.min]])
    remainder <- length(lst[[xy.max]]) %% length(lst[[xy.min]])
    if.zero   <- !is.na(remainder/remainder) %>% sum()

    xy.quotient  <- rep(lst[[xy.min]], quotient)
    xy.remainder <- rep(lst[[xy.min]][1L:remainder], if.zero)

    lst[[xy.min]] <- c(xy.quotient, xy.remainder)
  }
  return(lst)
}


# remap_dots_TRUE() ---------------------------------------------------------


remap_dots_TRUE <- function(lst) {
  lst <- as.list(lst)
  if (lst$is.dots) {
    start <- list.pos("is.dots", lst) + 1
    end   <- length(lst)

    dots <- expand.grid(lst[end:start], stringsAsFactors = FALSE)
    lst[end:start] <- dots[1L:length(dots)]
  }
  return(lst)
}


# remap_dots_FALSE() --------------------------------------------------------


remap_dots_FALSE <- function(lst) {
  lst <- as.list(lst)

  if (lst$is.dots) {
    start <- list.pos("is.dots", lst) + 1
    end   <- length(lst)
    dots  <- lst[start:end]

    recycled.index <- sapply(dots, length) %>% which.max()
    recycled       <- lapply(dots[-recycled.index], function(x, y) {
      x[1L:length(dots[[y]])]
      }, y = recycled.index)
    lst[names(recycled)] <- recycled
  }
  return(lst)
}


# remap_dots_NA() ---------------------------------------------------------


remap_dots_NA <- function(lst) {
  # lst$is.dots.chained <- FALSE
  return(lst)
}

