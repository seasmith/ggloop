
# mplot() ---------------------------------------------------------
#'
#' @title
#' Plot multiple plots in one layout.
#'
#' @description
#' \code{ggplot} objects can be passed in ..., or to plotlist (as a list of
#' ggplot objects) - cols:   Number of columns in layout - layout: A matrix
#' specifying the layout. If present, 'cols' is ignored.
#'
#' @param plotlist A list of \code{ggplot} plots (if you prefer).
#' @param file Not yet known/defined.
#' @param cols Number of columns in the layout.
#' @param layout A matrix specifying the layout. If present, \code{cols} is
#'   ignored.
#'
#' @details
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then
#' plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go
#' all the way across the bottom.
#'
#' @seealso
#' The source for the code is Winston Chang's "Cookbook for R", and can be found
#' at the following link:
#' \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#'
#' A list of StackOverflow Q&A's:
#' \itemize{
#' \item \href{http://stackoverflow.com/questions/13788563/r-multiplot-not-working-because-of-error-in-usemethodgetmodelinfo-model}{R: multiplot not working because of error in UseMethod(“getModelInfo”,model)}
#' \item \href{http://stackoverflow.com/questions/11721401/r-save-multiplot-to-file}{R - save multiplot to file}
#' \item \href{http://stackoverflow.com/questions/8615530/place-title-of-multiplot-panel-with-ggplot2}{Place title of multiplot panel with ggplot2}
#' \item \href{http://stackoverflow.com/questions/28138257/r-multiplot-write-plot-to-file-grid-package}{R multiplot write plot to file grid package}
#' }

mplot <- function(plotlist=NULL, file, cols=1, layout=NULL) {
  # library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout),
                                                           ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}
