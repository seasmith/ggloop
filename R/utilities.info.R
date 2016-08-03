aes_info <- function(lst){
  top.level    <- summary(list(lst))
  mid.level <- summary(lst)
  base.level   <- lapply(g, summary)
  new.lst <- list(Top = top.level,
                  Mid = mid.level,
                  Base = base.level)
  return(new.lst)
}
