# aes_loop() --------------------------------------------------------------
#' Create a grouped list of uneval aesthetics to pass to \code{ggloop()}.
#'
#' This version of \code{aes_loop} is to be used inside \code{ggloop()}. If
#' you wish to return the grouped list of uneval aesthetics, then use
#' \code{aes_loop2()}.
#' @export

aes_loop <- function(x, y, ..., remap_xy = TRUE, remap_dots = FALSE){

  # handle the first set of arguments
  if(!missing(x)) x <- substitute(x)
  if(!missing(y)) y <- substitute(y)
  dots <- as.list(substitute(list(...)))[-1L]

  function(vars, remap_xy, remap_dots){
    # create stashing environment to return to the mother
    e <- new.env()

    aes.raw <- aes_eval(vars, x, y, dots)

    # remap_xy
    if(is.na(remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else{
      if(remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else{
        if(!remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
      }
    }

    # remap_dots
    if(is.na(remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else{
      if(remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else
        if(!remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
    }
      # stash
      e$aes.raw <- aes.raw

    aes.grouped <- aes_group(aes.raw) %>% rename_inputs()

      # stash
      e$xy <- xy
      e$rep.num <- rep.num
      e$dots.vector <- dots.vector

    if(is.null(rep.num) && is.null(dots.vector)){
      aes.inputs.dirty <- extract(xy, lengths(xy)[[1]])

      aes.inputs.clean <- lapply(aes.inputs.dirty, function(x){
        x[which(!is.na(x))]
      })

      aes.list <- mapply(map_aes, aes.inputs.clean, SIMPLIFY = FALSE)

        # stash
        e$aes.list <- aes.list
    } else{
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
    }
    return(e)
  }

}



# aes_loop2() -------------------------------------------------------------
#' @export

aes_loop2 <- function(data, x, y, ..., remap_xy = TRUE, remap_dots = FALSE){

  # set dplyr::select_vars_() variables
  if(is.data.frame(data)) vars <- names(data)
  else if(is.character(data)) vars <- data
  else stop("data argument is not of correct type: must be either data
            frame or character vector")

  if(!missing(x)) x <- substitute(x)
  if(!missing(y)) y <- substitute(y)
  dots <- as.list(substitute(list(...)))[-1L]

  aes.raw <- aes_eval(vars, x, y, dots)

  # remap_xy
  if(is.na(remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else{
    if(remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else{
      if(!remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
    }
  }

  # remap_dots
  if(is.na(remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else{
    if(remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else
      if(!remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
  }

  aes.grouped <- aes_group(aes.raw) %>% rename_inputs()

  if(!aes.raw[["is.dots"]]){
    aes.inputs <- extract(xy, lengths(xy)[[1]])

    aes.list <- mapply(map_aes, aes.inputs, SIMPLIFY = F)
  } else{
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
  }

  return(aes.list)

}
