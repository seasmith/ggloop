# create raw list from aes_loop() inputs
nputs.raw <- aes_assign(data = mtcars,
                        x = c(mpg, 2, disp:hp),
                        y = mpg:hp,
                        color = am:gear,
                        size = carb)

# remap aes mappings
nputs.staged <- nputs.raw  %>% remap_xy_FALSE() %>% remap_dots_TRUE()

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
    ggplot(mtcars, aes.list[[x]][[y]])
  })})
