# I can't bring myself to throw out this code, just yet


# aes_loop() --------------------------------------------------------------
#' @export

aes_loop <- function(x, y, remap_xy, remap_dots, ...){
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
