
# extractor() -------------------------------------------------------------


extractor <- function(lst, num){
  rslt <- list()
  for(i in seq_len(num)){
    rslt[[i]] <- sapply(lst, `[[`, i)
  }
  return(rslt)
}


# set_aes() ---------------------------------------------------------------


set_aes <- function(...){
  lst <- list(...)
  lapply(lst, function(x){
    x <- parse(text = x)
    structure(x, class = "uneval")
  })
}


# name_aes() ------------------------------------------------------------


names_aes <- function(lst){
  to.name <- lapply(lst, function(x){
    gsub("[0-9]+$", "", names(x))
  })

  for(z in seq_along(lst)){
    for(x in seq_along(lst[[z]])){
      for(y in seq_along(lst[[z]][[x]])){
        names(lst[[z]][[x]])[y] <- to.name[[z]][y]
      }
    }
  }
}
