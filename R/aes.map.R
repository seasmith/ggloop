
# map_aes() ---------------------------------------------------------------
#
#' Loop through a list of grouped variables and assign class "uneval" to each
#' element in the group.
#'
#' This is essentially \code{aes()} from \code{ggplot2} placed inside of an
#' \code{lapply()} loop. The function name is passed in an \code{mapply()} loop
#' inside of \code{aes_loop()} and \code{aes_loop2()}.
#'
#' @param lst A list of grouped variables to be assigned class \code{uneval}

map_aes <- function(lst) {
    mapping <- lapply(lst, function(x) {
      x <- parse(text = x)[[1L]]
    })
    structure(mapping, class = "uneval")
}



# name_groups() -----------------------------------------------------------
#
#' Extract names for the first level of list components for the returned value
#' of \code{ggloop()}.
#'
#' @param lst A list - specifically \code{aes.raw}.
#' @param dots.vector A vector corresponding to the position of the \code{...}
#'   arguments in the \code{aes.raw} list.

name_groups <- function (lst, dots.vector) {

  names.matrix <-  matrix(unlist(lst[dots.vector]),
                          ncol = length(dots.vector))
  colnames(names.matrix) <- names(lst[dots.vector])

  names.list <- sapply(seq_len(nrow(names.matrix)), function(x) {
    .names <- sapply(seq_len(ncol(names.matrix)), function(y) {
      if (!is.na(names.matrix[x, y])) {
        paste(colnames(names.matrix)[y],
              stats::na.omit(names.matrix[x, y]),
              sep = ".")
      } else {
        NA
      }
    })
    paste(stats::na.omit(.names), collapse = "_")
  })

  return(names.list)
}



# name_subgroups() --------------------------------------------------------
#
#' Extract names for the second level of list components for the returned value
#' of \code{ggloop()}.
#'
#' @param lst A list - specifically \code{xy}.
#' @param dots.vector A vector corresponding to the position of the \code{...}
#'   arguments in the \code{aes.raw} list.

name_subgroups <- function(lst, dots.vector){
  # lst = xy
  names.matrix <- matrix(unlist(lst), ncol = length(lst))
  colnames(names.matrix) <- names(lst)

  names.list <- sapply(seq_len(nrow(names.matrix)), function(x) {
    .names <- sapply(seq_len(ncol(names.matrix)), function(y) {
      if (!is.na(names.matrix[x, y])) {
        paste(colnames(names.matrix)[y],
              stats::na.omit(names.matrix[x, y]),
              sep = ".")
      } else {
        NA
      }
    })
    paste(stats::na.omit(.names), collapse = "_")
  })

  return(names.list)
}
