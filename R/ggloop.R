#' @importFrom methods hasArg
#' @include aes.loop.R
#' @include aes.map.R

# ggloop() ----------------------------------------------------------------
#
#' @title
#' Create ggplot plots in a loop.
#'
#' @description
#' \code{ggloop()} mimics \code{ggplot()} by excepting a data frame and mappings
#' and returning a plot. Only, \code{ggloop()} accepts vectors for aesthetics
#' and returns a list of grouped \code{ggplot} plots.
#'
#' @param data Default dataset to use for plot. Must be a data frame and can be
#'   only one data frame.
#' @param mappings Default list of aesthetic mappings to use for plots.
#' @param remap_xy Remapping behavior of \code{x} and \code{y} vectors specified
#'   in \code{aes_loop()}.
#' @param remap_dots Remapping behavior of \code{...} vectors specified in
#'   \code{aes_loop()}.
#' @param ... Other arguments, such as \code{map.list} which is set to
#'   \code{FALSE} by default.
#' @param environment An environment and only one environment (cannot be a
#'   vector). If a variable defined in the aesthetic mapping is not found in the
#'   data, \code{ggplot()} (called inside \code{ggloop()}) will look for it in
#'   this environment. It defaults to using the environment in which
#'   \code{ggloop()} was called.
#' @details \code{ggloop()} makes use of the first of two \code{aes_loop}
#' functions. \code{aes_loop()} is meant to mimic \code{aes} from
#' \code{ggplot2}, and because of this, the remapping behavior of the aesthetics
#' is controlled by the remapping arguments supplied to \code{ggloop()}.
#'
#' The first remapping argument, \code{remap_xy} can take three values:
#' \itemize{ \item \code{TRUE} = The default behavior. All unqiue combinations
#' of \code{x} and \code{y} are generated. This means that if a variable (i.e.
#' \code{mpg}) is supplied in both \code{x} and \code{y}, then no mapping will
#' have \code{x} and \code{y} variables that are the same (i.e. \code{x -> mpg;
#' y -> mpg} will not ever happen). Likewise, no unordered pair duplicates will
#' happen (i.e. \code{x -> mpg; y -> cyl} and \code{x -> cyl; y -> mpg} will be
#' treated the same). \item \code{FALSE} = If \code{x} and \code{y} vectors are
#' not the same length, then the shorter of the two will be recycled. Recycling
#' is similar to \code{mapply()}'s recycling. \item \code{NA} = If \code{x} and
#' \code{y} vectors are not the same length, then the shorter of the two will
#' have \code{NA} assigned to the missing elements. These are meant to act as
#' placeholders during the wrangling operations (extracting and grouping the
#' aesthetics), and will be taken out before the final list of mappings is sent
#' to \code{ggloop()}. }
#'
#' @export

ggloop <- function(data, mappings = aes_loop(), remap_xy = TRUE,
                   remap_dots = FALSE, ..., environment = parent.frame()){

  # Create names variable and evaluate `mappings` to give it ggloop() enclosure
  # so that it may use other ggloop() arguments.
  vars <- names(data)
  mappings <- eval(mappings)
  mappings <- mappings(vars, remap_xy, remap_dots)

  # Check if map.list argument was passed.
  if(methods::hasArg("map.list")) map.list <- list(...)$map.list
  else map.list <- FALSE

  # Return mappings$aes.list if map.list == TRUE
  if(eval(parse(text = map.list))) return(mappings$aes.list)

  # Loop.
  if(mappings$aes.raw[["is.dots"]]){
    gg.list <- lapply(seq_along(mappings$aes.list), function(x){
      lapply(seq_along(mappings$aes.list[[x]]), function(y){
        ggplot2::ggplot(data = data,
                        mapping = mappings$aes.list[[x]][[y]],
                        environment = environment)
      })
    })

    # Tidy-up the group names ("dots" names).
    names(gg.list) <- name_groups(mappings$aes.raw, mappings$dots.vector)

    # Tidy-up the subgroup names ("xy" names).
    for(i in seq_along(gg.list)){
      names(gg.list[[i]]) <- name_subgroups(mappings$xy, mappings$dots.vector)
    }

    return(gg.list)
  } else{
    gg.list <- lapply(seq_along(mappings$aes.list), function(x){
      ggplot2::ggplot(data = data,
                      mapping = mappings$aes.list[[x]],
                      environment = environment)
    })

    # No need to run name_groups since there are no "dots" in the FALSE case.
    names(gg.list) <- name_subgroups(mappings$xy, lengths(mappings$xy)[1])

    return(gg.list)
  }
}
