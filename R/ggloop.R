
# ggloop() ----------------------------------------------------------------
#' @export

ggloop <- function(data,
                   mappings = aes_loop(),
                   remap_xy = TRUE,
                   remap_dots = FALSE,
                   environment = parent.frame()){

  mappings.eval <- eval(mappings)
  mappings <- mappings.eval(data, remap_xy, remap_dots)

  if(mappings$aes.raw[["is.dots"]]){
    gg.list <- lapply(seq_along(mappings$aes.list), function(x){
      lapply(seq_along(mappings$aes.list[[x]]), function(y){
        ggplot2::ggplot(data = data,
                        mapping = mappings$aes.list[[x]][[y]],
                        environment = environment)
      })
    })

    names(gg.list) <- name_groups(mappings$aes.raw, mappings$dots.vector)

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

    names(gg.list) <- name_subgroups(mappings$xy, lengths(mappings$xy)[1])

    return(gg.list)
  }
}


# ggloop2() ---------------------------------------------------------------
#' @export

ggloop2 <- function(data,
                    mappings = aes_loop2(),
                    remap_xy = TRUE,
                    remap_dots = FALSE,
                    environment = parent.frame()){

  vars <- names(data)
  mappings.eval <- eval(mappings)
  mappings <- mappings.eval(vars, remap_xy, remap_dots)

  if(mappings$aes.raw[["is.dots"]]){
    gg.list <- lapply(seq_along(mappings$aes.list), function(x){
      lapply(seq_along(mappings$aes.list[[x]]), function(y){
        ggplot2::ggplot(data = data,
                        mapping = mappings$aes.list[[x]][[y]],
                        environment = environment)
      })
    })

    names(gg.list) <- name_groups(mappings$aes.raw, mappings$dots.vector)

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

    names(gg.list) <- name_subgroups(mappings$xy, lengths(mappings$xy)[1])

    return(gg.list)
  }
}
