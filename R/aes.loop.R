#' @include aes.wrangle.R
#' @include aes.remap.R

# aes_loop() --------------------------------------------------------------
#
#' @title
#' Create a list of grouped aesthetic mappings.
#'
#' @description
#' \code{aes_loop} cannot be used affectively outside of \code{ggloop()} (or at
#' least with access to the data frame names).
#'
#' @param x,y,... A vector of variable names. Vector can consist of a
#'   combination of \code{dplyr}-like symbols (unqouted names) and
#'   \code{numerics/integers} referencing the variable position within
#'   \code{data}.
#'
#' @details \code{aes_loop()} is solely meant to be called within
#' \code{ggloop()}. To create the raw list of grouped mappings, set
#' \code{ggloop()}'s \code{gg_obs} argument to \code{FALSE}.
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
  if(!missing(x)) x <- substitute(x) else x <- NULL
  if(!missing(y)) y <- substitute(y) else y <- NULL
  dots <- as.list(substitute(list(...)))[-1L]

  # Stop if no x or y has been supplied.
  if (all(is.null(x), is.null(y)))
    stop("You must supply at least one x or y aesthetic")

  # Write the function body to be assigned inside ggloop(). This will allow the
  # function body to have access to the other formal arguments declared in
  # ggloop().
  function(vars, remap_xy, remap_dots) {
    aes.raw <- aes_eval(x, y, dots, vars)

    # remap_xy precedence: NA -> TRUE -> FALSE
    aes.raw <- if (is.na(remap_xy)) remap_xy_NA(aes.raw) else
        if (remap_xy) remap_xy_TRUE(aes.raw) else
            if (!remap_xy) remap_xy_FALSE(aes.raw)

    # remap_dots precedence: NA -> TRUE -> FALSE
    aes.raw <- if (is.na(remap_dots)) remap_dots_NA(aes.raw) else
        if (remap_dots) remap_dots_TRUE(aes.raw) else
            if (!remap_dots) remap_dots_FALSE(aes.raw)

    # Create "group" combinations between the x,y and the dots.
    e <- aes_group(aes.raw)

    # stash
    e$aes.raw <- aes.raw

    # Create the aes.list to pass to ggloop() with or without "dots".
    if (e$aes.raw[["is.dots"]]) {
      aes.inputs.dirty <- lapply(e$groups, function(x) extract(x, e$rep.num))

      aes.inputs.clean <- lapply(aes.inputs.dirty, function(x) {
        lapply(x, function(y) y[which(!is.na(y))])
      })

      e$aes.list <- lapply(aes.inputs.clean, function(x) Map(map_aes, x))
  } else {
    aes.inputs.dirty <- extract(e$xy, lengths(e$xy)[[1]])

    aes.inputs.clean <- lapply(aes.inputs.dirty, function(x) {
      x[which(!is.na(x))]
    })

    e$aes.list <- Map(map_aes, aes.inputs.clean)
  }
    return(e)
  }

}
