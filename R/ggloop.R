
# ggloop() ----------------------------------------------------------------
#' @export

ggloop <- function(mappings = aes_loop(),
                   remap_xy = TRUE,
                   remap_dots = FALSE,
                   environment = parent.frame()){
  .remap_xy <- remap_xy
  .remap_dots <- remap_dots
  gg.list <- lapply(seq_along(mappings$aes.list), function(x){
    lapply(seq_along(mappings$aes.list[[x]]), function(y){
      ggplot2::ggplot(data = mappings$data,
                      mapping = mappings$aes.list[[x]][[y]],
                      environment = environment)
    })
  })

  names(gg.list) <- name_groups(mappings$aes.raw, mappings$dots.vector)

  for(i in seq_along(gg.list)){
    names(gg.list[[i]]) <- name_subgroups(mappings$xy, mappings$dots.vector)
  }
  return(gg.list)
}


# aes_loop() --------------------------------------------------------------
#' @export

aes_loop <- function(data, x, y, ...){
  # create stashing environment
  e <- new.env()
  e$data <- data

  pf <- parent.frame()

  # filter "data" argument out of ...
  # dots.list <- list(...)
  # dots.data <- which(names(dots.list) %in% "data")
  #   if(length(dots.data) > 1) stop("Only one 'data' argument allowed")
  # dots.list <- dots.list[-dots.data]

        # set dplyr::select_vars_() variables
        vars <- names(data)
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
        print(y.eval)

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
        aes.raw <- c(x = x.eval,
                      y = y.eval,
                      is.dots = is.dots, dots.eval)
        e$aes.raw <- aes.raw

  # place mapping argument values in a list
  # aes.raw <- aes_assign(.data, x, y, ...)


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(.remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else{
    if(.remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else{
      if(!.remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
    }
  }

  # remap TRUE/FALSE dots
  if(is.na(.remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else{
    if(.remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else
      if(!.remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
  }

  aes.grouped <- aes_group(aes.raw) %>% rename_inputs()
    # stash
    e$xy <- xy
    e$rep.num <- rep.num
    e$dots.vector <- dots.vector

  aes.inputs.dirty <- lapply(aes.grouped, function(x){
    extract(x, rep.num)
  })

  aes.inputs.clean <- lapply(aes.inputs.dirty, function(x){
    lapply(x, function(y){
      y[which(!is.na(y))]
    })
  })

  aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
    mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
  })
    #stash
    e$aes.list <- aes.list

  return(e)

}


