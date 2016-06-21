# I can't bring myself to throw out this code, just yet


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
