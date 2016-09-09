# I can't bring myself to throw out this code, just yet


# aes_loop() --------------------------------------------------------------
#' @export

# aes_loop <- function(x, y, remap_xy, remap_dots, ...){
#   # place mapping argument values in a list
#   mappings <- aes_assign(.data, x, y, ...)
#   e$mappings <- mappings
#
#   # remap TRUE/FALSE/NA xy value pairs
#   if(is.na(remap_xy)) mappings <- remap_xy_NA(mappings) else{
#     if(remap_xy) mappings <- remap_xy_TRUE(mappings) else{
#       if(!remap_xy) mappings <- remap_xy_FALSE(mappings)
#     }
#   }
#
#   # remap TRUE/FALSE dots
#   if(is.na(remap_dots)) mappings <- remap_dots_NA(mappings) else{
#     if(remap_dots) mappings <- remap_dots_TRUE(mappings) else
#       if(!remap_dots) mappings <- remap_dots_FALSE(mappings)
#   }
#
#   mappings.grouped <- aes_group(mappings) %>% rename_inputs()
#
#   aes.inputs.raw <- lapply(mappings.grouped, function(x){
#     extract(x, rep.num)
#   })
#
#   aes.inputs.clean <- lapply(aes.inputs.raw, function(x){
#     lapply(x, function(y){
#       y[which(!is.na(y))]
#     })
#   })
#
#   aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
#     mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
#   })
# }



# aes_inputs() ------------------------------------------------------------


# aes_inputs <- function(data, x, y, ...){
#   # capture x values if exist
#   if(hasArg(x)){
#     x <- substitute(x)
#     x.eval <- data %>% names() %>% dplyr::select_vars(eval(x)) %>%
#       unname() %>% list()
#     is.x <- TRUE
#   } else{
#     x.eval <- NULL
#     is.x   <- FALSE
#   }
#
#   #capture y values if exist
#   if(hasArg(y)){
#     y <- substitute(y)
#     y.eval <- data %>% names() %>% dplyr::select_vars(eval(y)) %>%
#       unname() %>% list()
#     is.y <- TRUE
#   } else{
#     y.eval <- NULL
#     is.y   <- FALSE
#   }
#
#   # capture dots if exist
#   dots <- as.list(substitute(list(...)))[-1L]
#   if(length(dots) > 0){
#     dots.eval <- lapply(seq_along(dots), function(i){
#       arg.eval <- data %>% dplyr::select_vars(eval(dots[[i]])) %>%
#         unname()
#     }) %>%
#       magrittr::set_names(names(dots))
#     is.dots <- TRUE
#   } else{
#     dots.eval <- NULL
#     is.dots   <- FALSE
#   }
#
#   # list logical existance and values (if any) for all arguments
#   mappings <- c(is.x = is.x, x = x.eval,
#                 is.y = is.y, y = y.eval,
#                 is.dots = is.dots, dots.eval)
#   return(mappings)
# }

# aes_list2() -------------------------------------------------------------

# aes_list2 <- function(lst){
#   # test for existence and set length to replicate dots (rep.length)
#   if(lst$is.x) rep.length <- length(lst$x)
#   if(lst$is.y) rep.length <- length(lst$y)
#   if(!lst$is.x && !lst$is.y) rep.length <- ## fill this in later
#
#       new.lst <- list()
#
#   start <- which((names(lst) %in% "is.dots")) + 1
#   end <- length(lst) - 1
#   dots <- lst[start:end]
#   lapply(seq_along(dots), function(x){
#     dots
#   })
# }

# aes_list() --------------------------------------------------------------


# aes_list <- function(lst){
#   dots.num <- length(lst) - 2
#   dots.length <- length(lst[3])
#   lapply(dots.length, function(x){
#     Map(f = aes, x = lst$x, y = lst$y, lst)
#   })
#
#   mapply(FUN = ggplot2::aes,
#          x = x,
#          y = y,
#          ... = ...,
#          SIMPLIFY = F)
# }

# aes_matrix() ------------------------------------------------------------


# aes_matrix <- function(lst){
#   # if exists(xy)
#   xy <- cbind(x = lst$x, y = lst$y)
#
#   start <- which((names(lst) %in% "is.dots")) + 1
#   end <- length(lst) - 1
#   dots <- lst[start:end] %>% unlist() %>%
#     matrix(ncol = length(start:end)) %>%
#     magrittr::set_colnames(names(lst[start:end]))
#
#   matrices <- lapply(seq_along(start:end), function(i){
#     new.dots <- dots[rep(i, dim(xy)[1]), ]
#     cbind(xy, new.dots)
#   })
#
#   return(matrices)
#
#   # if  exists(xy) one or none
# }

# aes_loop2() -------------------------------------------------------------
#' @export

# aes_loop2 <- function(data, x, y, remap_xy, remap_dots, ...){
#   # place mapping argument values in a list
#   mappings <- aes_assign(data, x, y, ...)
#
#     # Logic vector indicating value of remap_xy argument
#     logic_xy <- c(isTRUE(remap_xy),
#                   isFALSE(remap_xy),
#                   is.na(remap_xy))
#     # Logic vector of the three possible functions to run
#     FUN_xy <- list(if_TRUE  = remap_xy_TRUE,
#                    if_FALSE = remap_xy_FALSE,
#                    if_NA    = remap_xy_NA)
#   # Subset FUN_xy to run the appropriate function
#   mappings <- FUN_xy[[which(logic_xy)]](mappings)
#
#
#     # Logic vector indicating value of remap_xy argument
#     logic_dots <- c(isTRUE(remap_dots),
#                     isFALSE(remap_dots))
#     # Logic vector of the two possible functions to run
#     FUN_dots <- list(if_TRUE = remap_dots_TRUE,
#                      if_FALSE = remap_dots_FALSE)
#   # Subset FUN_dots to run the appropriate function
#   mappings <- FUN_dots[[which(logic_dots)]](mappings)
#
#
# }

# aes_group2() ------------------------------------------------------------
#
#' Create unique pairings between \code{c(x, y)} and \code{dots}.
#'
#' \code{aes_group()} uses a list of \code{x's} and \code{y's} to create each
#' unique combination with \code{dots}. The difference between
#' \code{aes_group()} and \code{aes_group2()} is how they create unqiue
#' combinations. \code{aes_group2()} takes each unique \code{x}, \code{y}
#' combination and assigns all \code{dots} to that unique combination.
#'
#' \code{aes_group()} does the opposite in that it takes a list of all \code{x}
#' and \code{y} variables and assings a unique \code{dots} argument. In this
#' sense, if there are multiple variables assigned to a \code{dot} (like
#' \code{colour}, then \code{aes_group()} will take a list of all \code{x} and
#' \code{y} variables and add to it \code{colour.N} where \code{.N} denotes the
#' number of variables assigned to \code{colour}.
#'
#' @param lst A list. The list that will be passed to \code{aes_group()} will be
#'   the list produced by \code{aes_assing()}.

# aes_group2 <- function(lst){
#
#   # MUST WRITE CODE TO DEAL WITH CIRCUMSTANCE OF NO X AND NO Y
#   if(lst$is.x) rep.num <- length(lst$x) else
#     if(lst$is.y) rep.num <- length(lst$y) # else
#     # more code
#
#     start <- which((names(lst) %in% "is.dots")) + 1
#     end <- length(lst) - 1
#     dots.vector <- start:end
#
#     # a summary table of name, length, and order of list elements
#     summ <- summary(lst) %>% as.data.frame.matrix()
#     summ$Name <- rownames(summ)
#     summ <- summ[ , c(4, 1:3)]
#
#     # use summ (summary) table to find x and y
#     x.pos <- which(summ$Name %in% "x")
#     if(sum(x.pos) > 0) x.length <- summ$Length["x"]
#     y.pos <- which(summ$Name %in% "y")
#     if(sum(y.pos) > 0) y.length <- summ$Length["y"]
#
#     xy <- c(lst$is.x*x.pos, lst$is.y*y.pos) %>%
#       lst[.] %>% matrix(unlist(.), ncol = length(.))
#
#     dots.list <- lapply(unlist(lst[dots.vector]), function(x, times){
#       rep(x, times)},
#       times = rep.num)
#
#     # vector.len <- length(dots.vector)
#     # list.len <- length(dots.list)
#
#     # nlst.lst <-  lapply(seq_len(list.len/vector.len), function(x){
#     #   unit.vector <- seq(from = 1,
#     #                      to = list.len,
#     #                      by = list.len/vector.len)
#     #   itertor <- unit.vector + x - 1
#     #   c(xy[iterator], dots.list)
#     # })
#
#
#
#     return(lst)
# }

  # is.fun() ----------------------------------------------------------------
  #
  #' Attempts to decipher if a function other than \code{c()} has been supplied as
  #' input. Returns the position of the possible non-\code{c} functions in
  #' \code{lst}.
  #'
  #' @param lst A list of inputs wrapped in \code{substitute()} and coerced to a
  #' list using \code{as.list()}.

  # is.fun <- function(lst){
  #   # pars <- lapply(fun.par, function(x) grep(x, lst))[[1L]]
  #   # is.c.TRUE <- sapply(lst[pars], function(x){
  #   #   x.parse <- parse(text = x)
  #   #   x.eval <- eval(x.parse[[1L]])
  #   #   is.c(x.eval)
  #   # })
  #   # pars[!is.c.TRUE]
  #   pars <- sapply(fun.par, function(x) grep(x, lst))
  #   names(pars) <- NULL
  #   unique(pars)
  #   if(is.list(pars)) pars <- unlist(pars)
  #   pars
  # }
