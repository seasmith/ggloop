
# fomer aes_eval() --------------------------------------------------------
#

aes_eval <- function(vars, x, y, dots){
  names_list <- setNames(as.list(seq_along(vars)), vars)

  # test if anything was actually passed as x or y
  x.exists <- tryCatch({
    get0("x")
    TRUE
  }, error = function(e){
    FALSE
  })

  y.exists <- tryCatch({
    get0("y")
    TRUE
  }, error = function(e){
    FALSE
  })

  # capture x values if x exists
  if(x.exists){
    x.eval <- lazyeval::lazy_dots(eval(x)) %>%
      lazyeval::as.lazy_dots() %>%
      lazyeval::lazy_eval(c(names_list, select_helpers)) %>%
      magrittr::extract2(1L) %>% vars[.] %>% list()
  } else{
    x.eval <- NULL
  }

  # capture y values if y exists
  if(y.exists){
    y.eval <- lazyeval::lazy_dots(eval(y)) %>%
      lazyeval::as.lazy_dots() %>%
      lazyeval::lazy_eval(c(names_list, select_helpers)) %>%
      magrittr::extract2(1L) %>% vars[.] %>% list()
  } else{
    y.eval <- NULL
  }


  # capture dots if exist
  if(length(dots) > 0){
    dots.eval <- lapply(seq_along(dots), function(i){
      arg.eval <- lazyeval::lazy_dots(eval(dots[[i]])) %>%
        lazyeval::as.lazy_dots() %>%
        lazyeval::lazy_eval(c(names_list, select_helpers)) %>%
        magrittr::extract2(1L) %>% vars[.]
    }) %>%
      magrittr::set_names(names(dots))
    is.dots <- TRUE
  } else{
    dots.eval <- NULL
    is.dots   <- FALSE
  }


  # list values and logical existance of ... arguments
  mappings <- c(x = x.eval,
                y = y.eval,
                is.dots = is.dots,
                dots.eval)

  return(mappings)
}
