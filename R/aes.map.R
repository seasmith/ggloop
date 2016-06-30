
# extract() ---------------------------------------------------------------


extract <- function(lst, num){
  rslt <- list()
  for(i in seq_len(num)){
    rslt[[i]] <- sapply(lst, `[[`, i)
  }
  return(rslt)
}


# map_aes() ---------------------------------------------------------------


map_aes <- function(lst){
    mapping <- lapply(lst, function(x){
      x <- parse(text = x)[[1]]
    })
    structure(mapping, class = "uneval")
}



# name_groups() -----------------------------------------------------------


name_groups <- function(lst.1, lst.2, lst.3, dots.vector){
  # lst.1 = nlst
  # lst.2 = aes.inputs
  # lst.3 = nputs.staged
  # dots.vector = dots.vector
  lapply(seq_along(lst.1), function(x){
    names(lst.2)[x] <- lst.1[[x]] %>% unique()
  })

  .ncol <- as.numeric(summary(lst.3[dots.vector])[1])
  lst.names <-  matrix(unlist(lst.3[dots.vector]),
                        ncol = as.numeric(summary(lst.3[dots.vector])[1]))
}
