
# other.extractor() -------------------------------------------------------


# lst.length <- lapply(seq_along(lst), function(x){
#   length(x)
# })

# lapply(seq_along(lst), function(x){
#   sapply(lst[[x]], `[[`, lst.length[x])
# })

# mapply(`[[`, lst[[1]], 1:lst.length)


# extractor() -------------------------------------------------------------


# this is a force; for loop should be replaced with lapply()
extractor <- function(lst, num){
  rslt <- list()
  for(i in seq_len(num)){
    rslt[[i]] <- sapply(lst, `[[`, i)
  }
  return(rslt)
}


# set_aes() ---------------------------------------------------------------


set_aes <- function(...)
{
  lst <- list(...)
  lapply(lst, function(x){
    x <- parse(text = x)
    structure(x, class = "uneval")
  })
}


# name_aes() ------------------------------------------------------------


to.name <- lapply(lst, function()x{
  gsub("[0-9]+$", "", names(x))
})

for(z in seq_along(aes.list)){
  for(x in seq_along(aes.list[[z]])){
    # rename <-  gsub("[0-9]+$", "", names(aes.list[[z]][x]))
    for(y in seq_along(aes.list[[z]][[x]])){
      names(aes.list[[z]][[x]])[y] <- to.name[[z]][y]
    }
  }
}


# aes_map() ---------------------------------------------------------------


# aes_structure() ---------------------------------------------------------

