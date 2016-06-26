
# .all_aesthetics ---------------------------------------------------------


.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
                     "colour", "fg", "fill", "group", "hjust", "label",
                     "linetype", "lower", "lty", "lwd", "max", "middle",
                     "min", "pch", "radius", "sample", "shape", "size",
                     "srt", "upper", "vjust", "weight", "width", "x",
                     "xend", "xmax", "xmin", "xintercept", "y", "yend",
                     "ymax", "ymin", "yintercept", "z")


# .base_to_ggplot ---------------------------------------------------------


.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)


# rename_aes() ------------------------------------------------------------


rename_aes <- function(x){
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]

  plyr::rename(x, .base_to_ggplot, warn_missing = FALSE)
}


# rename_inputs() ---------------------------------------------------------


# rename_inputs <- function(lst){
#   for(z in seq_along(lst)){
#     for(x in seq_along(lst[[z]])){
#       names(lst[[z]][[x]]) <- gsub("[0-9]+$",
#                                    "",
#                                    names(lst[[z]][[x]]))
#     }
#   }
#   return(lst)
# }


# rename_inputs2() --------------------------------------------------------


rename_inputs2 <- function(lst){
    for(x in seq_along(lst)){
      for(y in seq_along(lst[[x]])){
        names(lst[[x]])[y] <- gsub("[0-9]+$",
                                   "",
                                   names(lst[[x]][y]))

        names(lst[[x]])[y] <- names(rename_aes(lst[[x]][y]))
      }
    }
    return(lst)
  }
