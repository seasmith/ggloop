
# map_aes() ---------------------------------------------------------------
#' Loop through a list an assing "uneval" class to each parsed element.
#'
#' This is essentially the core of \code{aes()} from \code{ggplot2} placed
#' inside of an \code{lapply()} loop. This function will be called using an
#' \code{mapply()} loop.

map_aes <- function(lst){
    mapping <- lapply(lst, function(x){
      x <- parse(text = x)[[1L]]
    })
    structure(mapping, class = "uneval")
}



# name_groups() -----------------------------------------------------------
#' Extract names for the first level of list components for the returned value
#' of \code{ggloop()}.

name_groups <- function(lst, dots.vector){

  names.matrix <-  matrix(unlist(lst[dots.vector]),
                          ncol = length(dots.vector))
  colnames(names.matrix) <- names(lst[dots.vector])

  names.list <- sapply(seq_len(nrow(names.matrix)), function(x){
    .names <- sapply(seq_len(ncol(names.matrix)), function(y){
      if(!is.na(names.matrix[x, y])){
        paste(colnames(names.matrix)[y],
              na.omit(names.matrix[x, y]),
              sep = ".")
      } else {
        NA
      }
    })
    paste(na.omit(.names), collapse = "_")
  })

  return(names.list)
}



# name_subgroups() --------------------------------------------------------
#' Extract names for the second level of list components for the returned value
#' of \code{ggloop()}.

name_subgroups <- function(lst, dots.vector){
  # lst = xy
  names.matrix <- matrix(unlist(lst), ncol = length(lst))
  colnames(names.matrix) <- names(lst)

  names.list <- sapply(seq_len(nrow(names.matrix)), function(x){
    .names <- sapply(seq_len(ncol(names.matrix)), function(y){
      if(!is.na(names.matrix[x, y])){
        paste(colnames(names.matrix)[y],
              na.omit(names.matrix[x, y]),
              sep = ".")
      } else {
        NA
      }
    })
    paste(na.omit(.names), collapse = "_")
  })

  return(names.list)
}
