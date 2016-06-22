
# extract() ---------------------------------------------------------------


extract <- function(lst, num){
  rslt <- list()
  for(i in seq_len(num)){
    rslt[[i]] <- sapply(lst, `[[`, i)
  }
  return(rslt)
}


# set_aes() ---------------------------------------------------------------


map_aes <- function(lst){
    mapping <- lapply(lst, function(x){
      x <- parse(text = x)[[1]]
    })
    structure(mapping, class = "uneval")
}
