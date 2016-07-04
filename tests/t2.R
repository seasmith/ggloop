aes.raw <- aes_assign(data = mtcars,
                      x = mpg:hp,
                      y = mpg:hp,
                      color = am:carb,
                      fill = carb,
                      size = gear) %>%
  remap_xy_FALSE() %>%
  remap_dots_FALSE()

aes.raw.grouped <- aes_group(aes.raw)
aes.raw.grouped <- rename_inputs(aes.raw.grouped)

aes.inputs <- lapply(aes.raw.grouped, function(x){
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

aes.list <- lapply(seq_along(aes.inputs), function(x){
  mapply(map_aes, aes.inputs[[x]], SIMPLIFY = FALSE)
})

g <- lapply(seq_along(aes.list), function(x){
  lapply(seq_along(aes.list[[x]]), function(y){
    ggplot2::ggplot(mtcars, aes.list[[x]][[y]])
  })})

names(g) <- name_groups(aes.raw, dots.vector)

for(i in seq_along(g)){
  names(g[[i]]) <- name_subgroups(xy, dots.vector)
}


