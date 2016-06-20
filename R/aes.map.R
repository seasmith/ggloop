
# jibberish now -----------------------------------------------------------



lapply(seq_along(dots), function(x){
  paste0(names(dots), "=", eval(parse(text = dots[x])))
})

lapply(seq_along(dots), function(x){
  paste0(names(dots), "=", "dots$", names(dots))
  match.call()[-1] %>% print
})



# mcall2 <- function(...)
# {
#   structure(as.list(match.call()[-1]), class = "uneval")
# }

# mcall3 <- function(...)
# {
#   lst <- list(...)
#   structure(lst, class = "uneval")
# }


# set_aes() ---------------------------------------------------------------


set_aes <- function(...)
{
  lst <- list(...)
  lapply(lst, function(x){
    x <- parse(text = x)
    structure(x, class = "uneval")
  })
}


# aes_map() ---------------------------------------------------------------

lst.length <- lapply(seq_along(lst), function(x){
  length(x)
})

lapply(seq_along(lst), function(x){
  sapply(lst[[x]], `[[`, lst.length[x])
})

# mapply(`[[`, lst[[1]], 1:lst.length)

# do this later
aes.deck <- lapply(lst, function(x){
  do.call(set_aes, x)
})

# rename
for(z in seq_along(aes.deck)){
  for(x in seq_along(aes.deck[[z]])){
    rename <-  gsub("[0-9]+$", "", names(aes.deck[[z]][x]))
    for(y in seq_along(aes.deck[[z]][[x]])){
      names(aes.deck[[z]][[x]])[y] <- rename
    }
  }
}

# aes_structure() ---------------------------------------------------------


