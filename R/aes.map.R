
# map_aes() ---------------------------------------------------------------


map_aes <- function(lst){
    mapping <- lapply(lst, function(x){
      x <- parse(text = x)[[1L]]
    })
    structure(mapping, class = "uneval")
}



# name_groups() -----------------------------------------------------------


name_groups <- function(lst, dots.vector){
  # lst.1 = nlst
  # lst.2 = aes.inputs
  # lst.3 = aes.raw
  # dots.vector = dots.vector

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
