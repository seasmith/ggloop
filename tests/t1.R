# create raw list from aes_loop() inputs
nputs.raw <- aes_inputs2(data = mtcars,
                     x = c(mpg, 2, disp:hp),
                     y = mpg:hp,
                     color = am:gear,
                     lty = am,
                     size = carb)

# remap aes mappings
nputs.staged <- nputs  %>% remap_xy_TRUE() %>% remap_dots_TRUE()

  # create vector that identifies the dots ("...") arguments
  start <- which((names(nputs.staged) %in% "is.dots")) + 1
  end <- length(nputs.staged) - 1
  dots.vector <- start:end

  # tell rep how many time to repeat x
  if(nputs.staged$is.x) rep.num <- length(nputs.staged$x) else
    if(lnputs.staged$is.y) rep.num <- length(nputs.staged$y)

# create a list of replicated dots pairs
nlst <- lapply(unlist(nputs.staged[dots.vector]), function(x, times){
  rep(x, times)},
  times = rep.num)

  # create xy vector from $is.x and $is.y; should 2 and 4 be defined?
  xy <- c(nputs.staged$is.x*2, nputs.staged$is.y*4) %>% nputs.staged[.]

# nest the list of replicated dots into aes params groups
nlst.lst <-  lapply(seq_len(length(nlst)/length(dots.vector)), function(x){
  unit.vector <- seq(from = 1,
                     to = length(nlst),
                     by = length(nlst)/length(dots.vector))
  multiple <- unit.vector + x - 1
  c(xy, nlst[multiple])})


