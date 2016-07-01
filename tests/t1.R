# create raw list from aes_loop() inputs
nputs.raw <- aes_assign(data = mtcars,
                        x = mpg:hp,
                        y = mpg:hp,
                     color = am:gear,
                     fill = carb,
                     size = gear)

# remap aes mappings
nputs.staged <- nputs.raw  %>% remap_xy_TRUE() %>% remap_dots_FALSE()

  # create vector that identifies the dots ("...") arguments
  start <- which((names(nputs.staged) %in% "is.dots")) + 1
  end <- length(nputs.staged) - 1
  dots.vector <- start:end

  # tell rep how many time to repeat x
  if(nputs.staged$is.x) rep.num <- length(nputs.staged$x) else
    if(nputs.staged$is.y) rep.num <- length(nputs.staged$y)

# create a list of replicated dots pairs
nlst <- lapply(unlist(nputs.staged[dots.vector]), function(x, times){
  rep(x, times)},
  times = rep.num)

  # a summary table of name, length, and order of list elements
  summ <- summary(nputs.staged) %>% as.data.frame.matrix()
  summ$Name <- rownames(summ)
  summ <- summ[ , c(4, 1:3)]

  # use summ (summary) table to find x and y
  x.pos <- which(summ$Name %in% "x")
  if(sum(x.pos) > 0) x.length <- summ$Length["x"]
  y.pos <- which(summ$Name %in% "y")
  if(sum(y.pos) > 0) y.length <- summ$Length["y"]

  # create xy vector from $is.x and $is.y; should 2 and 4 be defined?
  xy <- c(nputs.staged$is.x*x.pos, nputs.staged$is.y*y.pos) %>%
    nputs.staged[.]

# nest the list of replicated dots into aes params groups
nlst.lst <-  lapply(seq_len(length(nlst)/length(dots.vector)), function(x){
  unit.vector <- seq(from = 1,
                     to = length(nlst),
                     by = length(nlst)/length(dots.vector))
  multiple <- unit.vector + x - 1
  c(xy, nlst[multiple])})

  # rename
  nlst.lst <- rename_inputs2(nlst.lst)

# set the list with vectors to be sent to aes
aes.inputs <- lapply(nlst.lst, function(x){
  extract(x, rep.num)
})

      # Eliminate NA's in aes.inputs
      # STILL IN PRODUCTION
      # Produces character vector of length zero
      # if all vector elements are NA == TRUE
      aes.inputs <- lapply(aes.inputs, function(x){
        lapply(x, function(y){
          y[which(!is.na(y))]
        })
      })

# the unnamed aes list
aes.list <- lapply(seq_along(aes.inputs), function(x){
  mapply(map_aes, aes.inputs[[x]], SIMPLIFY = F)
  })
# deprecated
# aes.list <- lapply(aes.inputs, function(x){
#   do.call(set_aes, x)
# })

    # deprecated
    # # give aes.list some names
    # for(z in seq_along(aes.list)){
    #   for(x in seq_along(aes.list[[z]])){
    #     # rename <-  gsub("[0-9]+$", "", names(aes.list[[z]][x]))
    #     for(y in seq_along(aes.list[[z]][[x]])){
    #       names(aes.list[[z]][[x]])[y] <- to.name[[z]][y]
    #     }
    #   }
    # }

# put it all in a list
g <- lapply(seq_along(aes.list), function(x){
  lapply(seq_along(aes.list[[x]]), function(y){
    ggplot2::ggplot(mtcars, aes.list[[x]][[y]])
  })})


# renaming g --------------------------------------------------------------

# .ncol <- as.numeric(summary(nputs.staged[dots.vector])[1])
.ncol <- length(dots.vector)
names.matrix <-  matrix(unlist(nputs.staged[dots.vector]),
                     ncol = length(dots.vector))
colnames(names.matrix) <- names(nputs.staged[dots.vector])

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

names(g) <- name_groups(nputs.staged, dots.vector)
