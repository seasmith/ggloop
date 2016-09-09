#' @include aes.wrangle.R
#' @include aes.remap.R

# aes_loop() --------------------------------------------------------------
#
#' @title
#' Create a list of grouped aesthetic mappings.
#'
#' @description
#' This version of \code{aes_loop} is to be used inside \code{ggloop()}. If you
#' wish to return the grouped list of uneval aesthetics, then use
#' \code{aes_loop2()}.
#'
#' @param x,y,... A vector of variable names. Vector can consist of a
#'   combination of \code{dplyr}-like symbols (unqouted names) and
#'   \code{numerics/integers} referencing the variable position within
#'   \code{data}.
#'
#' @details \code{aes_loop()} is solely meant to be called within
#' \code{ggloop()}. To create the raw list of grouped mappings, use
#' \code{aes_loop2()}.
#'
#' @return
#' \code{aes_loop()} returns an environment that includes \code{aes.list} (the
#' list of grouped aesthetic mappings used inside \code{ggloop()}) and a few
#' vectors used by other functions and \code{lapply()} loops for control (to
#' eliminate running duplicate code to return a result from a previously ran
#' function).
#'
#' \code{aes_loop2()} returns a list of grouped mappings. This is similar to a
#' bunch of \code{aes()} mappings in a list waiting to be passed to
#' \code{ggplot()}.
#'
#'
#' @export

aes_loop <- function(x, y, ...){

  # Handle the first set of arguments (x, y, ...).
  if(!missing(x)) x <- substitute(x)
  if(!missing(y)) y <- substitute(y)
  dots <- as.list(substitute(list(...)))[-1L]

  function(vars, remap_xy, remap_dots){
    aes.raw <- aes_eval(vars, x, y, dots)

    # remap_xy precedence: NA -> TRUE -> FALSE
    if(is.na(remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else{
      if(remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else{
        if(!remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
      }
    }

    # remap_dots precedence: NA -> TRUE -> FALSE
    if(is.na(remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else{
      if(remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else
        if(!remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
    }

    e <- aes_group(aes.raw)

    # stash
    e$aes.raw <- aes.raw

    if(e$aes.raw[["is.dots"]]){
      aes.inputs.dirty <- lapply(e$groups, function(x){
        extract(x, e$rep.num)
      })

      aes.inputs.clean <- lapply(aes.inputs.dirty, function(x){
        lapply(x, function(y){
          y[which(!is.na(y))]
        })
      })

      # stash
      e$aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
        mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
      })
  } else{
    aes.inputs.dirty <- extract(e$xy, lengths(e$xy)[[1]])

    aes.inputs.clean <- lapply(aes.inputs.dirty, function(x){
      x[which(!is.na(x))]
    })

    # stash
    e$aes.list <- mapply(map_aes, aes.inputs.clean, SIMPLIFY = FALSE)
  }
    return(e)
  }

}



# aes_loop2() -------------------------------------------------------------
#
# #' @rdname aes_loop
# #' @param data Default dataset to use for plot. Must be a data frame and can be
# #'   only one data frame.
# #' @param remap_xy Remapping behavior of \code{x} and \code{y} vectors.
# #' @param remap_dots Remapping behavior of \code{...} vectors.
# #'
# #' @examples
# #' aes.list <- aes_loop2(data = mtcars, x = mpg:hp, y = disp, color =
# #' gear)
# #' print(aes.list)
# #'
# #' [[1]]
# #' [[1]][[1]]
# #' * x      -> mpg
# #' * y      -> disp
# #' * colour -> gear
# #'
# #' [[1]][[2]]
# #' * x      -> cyl
# #' * y      -> disp
# #' * colour -> gear
# #'
# #' [[1]][[3]]
# #' * x      -> hp
# #' * y      -> disp
# #' * colour -> gear
# #' @export

# aes_loop2 <- function(data, x, y, ..., remap_xy = TRUE, remap_dots = FALSE){
#
#   # set dplyr::select_vars_() variables
#   if(is.data.frame(data)) vars <- names(data)
#   else if(is.character(data)) vars <- data
#   else stop("data argument is not of correct type: must be either data
#             frame or character vector")
#
#   if(!missing(x)) x <- substitute(x)
#   if(!missing(y)) y <- substitute(y)
#   dots <- as.list(substitute(list(...)))[-1L]
#
#   aes.raw <- aes_eval(vars, x, y, dots)
#
#   # remap_xy
#   if(is.na(remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else{
#     if(remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else{
#       if(!remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
#     }
#   }
#
#   # remap_dots
#   if(is.na(remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else{
#     if(remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else
#       if(!remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
#   }
#
#   aes.grouped <- aes_group(aes.raw) %>% rename_inputs()
#
#   if(!aes.raw[["is.dots"]]){
#     aes.inputs <- extract(xy, lengths(xy)[[1]])
#
#     aes.list <- mapply(map_aes, aes.inputs, SIMPLIFY = F)
#   } else{
#     aes.inputs.dirty <- lapply(aes.grouped, function(x){
#       extract(x, rep.num)
#     })
#
#     aes.inputs.clean <- lapply(aes.inputs.dirty, function(x){
#       lapply(x, function(y){
#         y[which(!is.na(y))]
#       })
#     })
#
#     aes.list <- lapply(seq_along(aes.inputs.clean), function(x){
#       mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
#     })
#   }
#
#   return(aes.list)
#
# }
