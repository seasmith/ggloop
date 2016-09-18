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

aes_loop <- function(x, y, ...) {

  # Handle the first set of arguments (x, y, ...).
  if(!missing(x)) x <- substitute(x)
  if(!missing(y)) y <- substitute(y)
  dots <- as.list(substitute(list(...)))[-1L]

  # Write the function body to be assigned inside ggloop(). This will allow the
  # function body to have access to the other formal arguments declared in
  # ggloop().
  function(vars, remap_xy, remap_dots) {
    aes.raw <- aes_eval(vars, x, y, dots)

    # remap_xy precedence: NA -> TRUE -> FALSE
    if (is.na(remap_xy)) aes.raw <- remap_xy_NA(aes.raw) else {
      if (remap_xy) aes.raw <- remap_xy_TRUE(aes.raw) else {
        if (!remap_xy) aes.raw <- remap_xy_FALSE(aes.raw)
      }
    }

    # remap_dots precedence: NA -> TRUE -> FALSE
    if (is.na(remap_dots)) aes.raw <- remap_dots_NA(aes.raw) else {
      if (remap_dots) aes.raw <- remap_dots_TRUE(aes.raw) else {
        if (!remap_dots) aes.raw <- remap_dots_FALSE(aes.raw)
      }
    }

    # Create "group" combinations between the x,y and the dots.
    e <- aes_group(aes.raw)

    # stash
    e$aes.raw <- aes.raw

    if (e$aes.raw[["is.dots"]]) {
      aes.inputs.dirty <- lapply(e$groups, function(x) extract(x, e$rep.num))

      aes.inputs.clean <- lapply(aes.inputs.dirty, function(x) {
        lapply(x, function(y) y[which(!is.na(y))])
      })

      e$aes.list <- lapply(seq_along(aes.inputs.clean), function(x) {
        mapply(map_aes, aes.inputs.clean[[x]], SIMPLIFY = FALSE)
      })
  } else {
    aes.inputs.dirty <- extract(e$xy, lengths(e$xy)[[1]])

    aes.inputs.clean <- lapply(aes.inputs.dirty, function(x) {
      x[which(!is.na(x))]
    })

    e$aes.list <- mapply(map_aes, aes.inputs.clean, SIMPLIFY = FALSE)
  }
    return(e)
  }

}
