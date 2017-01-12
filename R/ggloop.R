#' @include aes.loop.R
#' @include aes.map.R

# ggloop() ----------------------------------------------------------------
#
#' @title
#' Create ggplot plots in a loop.
#'
#' @description
#' \code{ggloop()} mimics \code{ggplot()} by accepting both a data frame and
#' mappings, returning a plot - or plots in this case. The main difference is
#' that \code{ggloop()} accepts vectors for aesthetics and returns a list or
#' nested list of \code{ggplot} plots.
#'
#' @param data Default dataset to use for plot. Must be a data frame and can be
#'   only one data frame.
#' @param mappings List of aesthetic mappings to use for plots. Works like
#'   \code{mapping} from \code{ggplot()}.
#' @param remap_xy Remapping behavior of the \code{x} and \code{y} vectors
#'   specified in \code{aes_loop()}. See details below for more on remapping
#'   behavior.
#' @param remap_dots Remapping behavior of the \code{...} vectors specified in
#'   \code{aes_loop()}. See details below for more on remapping behavior.
#' @param gg_obs Logical. Specifies whether to return plots or the list (or
#'   nested list) of aesthetics used to make such a plots.
#' @param ... Other arguments. Similar to \code{ggplot()}'s \code{...}.
#' @param environment An environment and only one environment (cannot be a
#'   vector). Similar to \code{ggplot()}'s \code{environment}.
#'
#' @details
#' \code{ggloop()} makes use of \code{aes_loop}, which is meant to mimic
#' \code{aes} from \code{ggplot2}. Because of this, the remapping arguments are
#' supplied to \code{ggloop} instead of \code{aes_loop()}.
#'
#' The first remapping argument, \code{remap_xy} can take three values:
#' \itemize{
#'   \item{\code{TRUE} = The default behavior. All unqiue combinations of
#' \code{x} and \code{y} are generated. This means that if a variable (i.e.
#' \code{mpg}) is supplied in both \code{x} and \code{y}, then no mapping will
#' have \code{x} and \code{y} variables that are the same (i.e. \code{x -> mpg;
#' y -> mpg} will not ever happen). Likewise, no unordered pair duplicates will
#' happen (i.e. \code{x -> mpg; y -> cyl} and \code{x -> cyl; y -> mpg} will be
#' treated the same).}
#'   \item{\code{FALSE} = If \code{x} and \code{y} vectors are not the same
#' length, then the shorter of the two will be recycled. Recycling is similar to
#' \code{mapply()}'s recycling.}
#'   \item{\code{NA} = If \code{x} and \code{y} vectors are not the same length,
#' then the shorter of the two will have \code{NA} assigned to the missing
#' elements. These are meant to act as placeholders during the wrangling
#' operations (extracting and grouping the aesthetics), and will be taken out
#' before the final list of mappings is sent to \code{ggloop()}.}}
#'
#' @export
#' @examples
#' # 1. Return all possible x-y combinations.
#' plots <- ggloop(data = mtcars,
#'                 mappings = aes_loop(x = mpg:carb, y = mpg:carb))
#' names(plots)
#' # [1] "x.mpg_y.cyl"   "x.mpg_y.disp"  "x.mpg_y.hp"    "x.mpg_y.drat"
#' # [5] "x.mpg_y.wt"    "x.mpg_y.qsec"  "x.mpg_y.vs"    "x.mpg_y.am"
#' # [9] "x.mpg_y.gear"  "x.mpg_y.carb"  "x.cyl_y.disp"  "x.cyl_y.hp"
#' # [13] "x.cyl_y.drat"  "x.cyl_y.wt"    "x.cyl_y.qsec"  "x.cyl_y.vs"
#' # [17] "x.cyl_y.am"    "x.cyl_y.gear"  "x.cyl_y.carb"  "x.disp_y.hp"
#' # [21] "x.disp_y.drat" "x.disp_y.wt"   "x.disp_y.qsec" "x.disp_y.vs"
#' # [25] "x.disp_y.am"   "x.disp_y.gear" "x.disp_y.carb" "x.hp_y.drat"
#' # [29] "x.hp_y.wt"     "x.hp_y.qsec"   "x.hp_y.vs"     "x.hp_y.am"
#' # [33] "x.hp_y.gear"   "x.hp_y.carb"   "x.drat_y.wt"   "x.drat_y.qsec"
#' # [37] "x.drat_y.vs"   "x.drat_y.am"   "x.drat_y.gear" "x.drat_y.carb"
#' # [41] "x.wt_y.qsec"   "x.wt_y.vs"     "x.wt_y.am"     "x.wt_y.gear"
#' # [45] "x.wt_y.carb"   "x.qsec_y.vs"   "x.qsec_y.am"   "x.qsec_y.gear"
#' # [49] "x.qsec_y.carb" "x.vs_y.am"     "x.vs_y.gear"   "x.vs_y.carb"
#' # [53] "x.am_y.gear"   "x.am_y.carb"   "x.gear_y.carb"
#'
#' plots$x.mpg_y.hp + ggplot2::geom_point()
#'
#' # 2. Add an additional aesthetic (facet) to plots.
#' plots2 <- ggloop(data = mtcars,
#'                  mappings = aes_loop(
#'                                      x = c(disp, hp, wt),
#'                                      y = mpg,
#'                                      color = factor(cyl)))
#' sapply(plots2, names)
#' #      color.factor(cyl)
#' # [1,] "x.disp_y.mpg"
#' # [2,] "x.hp_y.mpg"
#' # [3,] "x.wt_y.mpg"
#'
#' plots2$`color.factor(cyl)`$x.hp_y.mpg + ggplot2::geom_point()
#'
#' # A look at remap_xy's other two behaviors:
#' # 3. remap_xy = NA
#' #    The longer vector will go "unpaired" after the shorter vector
#' #    runs out of elements.
#' plots3 <- ggloop(data = mtcars,
#'                  mappings = aes_loop(x = c(mpg/disp, mpg/hp, mpg/cyl, mpg/gear),
#'                                      y = c(hp, disp)),
#'                  remap_xy = NA)
#' names(plots3)
#' # [1] "x.mpg/disp_y.hp" "x.mpg/hp_y.disp" "x.mpg/cyl"       "x.mpg/gear"
#'
#' # 4. remap_xy = FALSE
#' #    The longer vector will be "paired" with the shorter vector using
#' #    recycling (similar to R's internal recycling, i.e. mapply()).
#' plots4 <- ggloop(data = mtcars,
#'                  mappings = aes_loop(x = c(mpg/disp, mpg/hp, mpg/cyl, mpg/gear),
#'                                      y = c(hp, disp)),
#'                  remap_xy = FALSE)
#' sapply(plots4, names)
#' # [1] "x.mpg/disp_y.hp"   "x.mpg/hp_y.disp"   "x.mpg/cyl_y.hp"    "x.mpg/gear_y.disp"
#'
ggloop <- function(data, mappings = aes_loop(), remap_xy = TRUE,
                   remap_dots = FALSE, gg_obs = TRUE, ...,
                   environment = parent.frame()) {

  # Create names variable and evaluate `mappings` to give it ggloop() enclosure
  # so that it may use other ggloop() arguments.
  vars <- names(data)
  mappings <- eval(mappings)
  mappings <- mappings(vars, remap_xy, remap_dots)

  # Check if user just wants the aes.list.
  if (!gg_obs) return(mappings$aes.list)

  # Loop.
  if (mappings$aes.raw[["is.dots"]]){
    gg.list <- lapply(mappings$aes.list, function(x) {
      gg.sub <- lapply(x, function(y) {
        ggplot2::ggplot(data        = data,
                        mapping     = y,
                        ...         = ...,
                        environment = environment)
      })
      structure(gg.sub, class = c("gglist"))
    })
    # Tidy-up the group names ("dots" names).
    names(gg.list) <- name_groups(mappings$aes.raw, mappings$dots.vector)

    # Tidy-up the subgroup names ("xy" names).
    for (i in seq_along(gg.list)) {
      names(gg.list[[i]]) <- name_subgroups(mappings$xy, mappings$dots.vector)
    }

    return(structure(gg.list, class = c("gglist", class(gg.list))))
  } else {
    gg.list <- lapply(mappings$aes.list, function(x) {
      ggplot2::ggplot(data        = data,
                      mapping     = x,
                      ...         = ...,
                      environment = environment)
    })

    # No need to run name_groups since there are no "dots" in the FALSE case.
    names(gg.list) <- name_subgroups(mappings$xy, lengths(mappings$xy)[1])

    return(structure(gg.list, class = c("gglist")))
  }
}
