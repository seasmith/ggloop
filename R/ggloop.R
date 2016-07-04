
# ggloop() ----------------------------------------------------------------
#' @export

ggloop <- function(data,
                   mapping = aes_loop3(),
                   remap_xy = TRUE,
                   remap_dots = FALSE,
                   environment = parent.frame()){
  .data <<- data
  lapply(seq_along(mapping), function(x){
    lapply(seq_along(mapping[[x]]), function(y){
      ggplot2::ggplot(data = data,
                      mapping = mapping[[x]][[y]],
                      environment = environment)
    })
  })
}


# aes_loop() --------------------------------------------------------------
#' @export

aes_loop <- function(x, y, remap_xy, remap_dots, ...){
  # place mapping argument values in a list
  mappings <- aes_assign(.data, x, y, ...)


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(remap_xy)) mappings <- remap_xy_NA(mappings) else{
      if(remap_xy) mappings <- remap_xy_TRUE(mappings) else{
          if(!remap_xy) mappings <- remap_xy_FALSE(mappings)
      }
    }

  # remap TRUE/FALSE dots
  if(is.na(remap_dots)) mappings <- remap_dots_NA(mappings) else{
      if(remap_dots) mappings <- remap_dots_TRUE(mappings) else
          if(!remap_dots) mappings <- remap_dots_FALSE(mappings)
    }

  mappings.grouped <- aes_group(mappings) %>% rename_inputs()

  aes.inputs.raw <- lapply(mappings.grouped, function(x){
    extract(x, rep.num)
  })

  aes.inputs.clean <- lapply(aes.inputs.raw, function(x){
    lapply(x, function(y){
      y[which(!is.na(y))]
    })
  })

  aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
    mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
  })
}



# aes_loop3() -------------------------------------------------------------
#' @export

aes_loop3 <- function(x, y, remap_xy, remap_dots, ...){
        # set dplyr::select_vars_() variables
        vars <- names(.data)
        names_list <- setNames(as.list(seq_along(vars)), vars)
        select_funs <- list(starts_with = function(...) starts_with(vars, ...),
                            ends_with = function(...) ends_with(vars, ...),
                            contains = function(...) contains(vars, ...),
                            matches = function(...) matches(vars, ...),
                            num_range = function(...) num_range(vars, ...),
                            one_of = function(...) one_of(vars, ...),
                            everything = function(...) everything(vars, ...))

        # capture x values if exist
        if(hasArg(x)){
          x <- substitute(x)
          x.eval <- lazyeval::lazy_dots(eval(x)) %>%
            lazyeval::as.lazy_dots() %>%
            lazyeval::lazy_eval(c(names_list, select_funs)) %>%
            magrittr::extract2(1L) %>% vars[.] %>% list()
        } else{
          x.eval <- NULL
        }

        # capture y values if exist
        if(hasArg(y)){
          y <- substitute(y)
          y.eval <- lazyeval::lazy_dots(eval(y)) %>%
            lazyeval::as.lazy_dots() %>%
            lazyeval::lazy_eval(c(names_list, select_funs)) %>%
            magrittr::extract2(1L) %>% vars[.] %>% list()
        } else{
          y.eval <- NULL
        }

        # capture dots if exist
        dots <- as.list(substitute(list(...)))[-1L]
        if(length(dots) > 0){
          dots.eval <- lapply(seq_along(dots), function(i){
            arg.eval <- lazyeval::lazy_dots(eval(dots[[i]])) %>%
              lazyeval::as.lazy_dots() %>%
              lazyeval::lazy_eval(c(names_list, select_funs)) %>%
              magrittr::extract2(1L) %>% vars[.]
          }) %>%
            magrittr::set_names(names(dots))
          is.dots <- TRUE
        } else{
          dots.eval <- NULL
          is.dots   <- FALSE
        }

        # list logical existance and values (if any) for all arguments
        mappings <- c(x = x.eval,
                      y = y.eval,
                      is.dots = is.dots, dots.eval)

  # place mapping argument values in a list
  # mappings <- aes_assign(.data, x, y, ...)


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(remap_xy)) mappings <- remap_xy_NA(mappings) else{
    if(remap_xy) mappings <- remap_xy_TRUE(mappings) else{
      if(!remap_xy) mappings <- remap_xy_FALSE(mappings)
    }
  }

  # remap TRUE/FALSE dots
  if(is.na(remap_dots)) mappings <- remap_dots_NA(mappings) else{
    if(remap_dots) mappings <- remap_dots_TRUE(mappings) else
      if(!remap_dots) mappings <- remap_dots_FALSE(mappings)
  }

  mappings.grouped <- aes_group(mappings) %>% rename_inputs()

  aes.inputs.raw <- lapply(mappings.grouped, function(x){
    extract(x, rep.num)
  })

  aes.inputs.clean <- lapply(aes.inputs.raw, function(x){
    lapply(x, function(y){
      y[which(!is.na(y))]
    })
  })

  aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
    mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
  })
}


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
