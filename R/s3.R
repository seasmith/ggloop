
# Generic - aes_eval() ----------------------------------------------------


#'
aes_eval <- function(maps, vars) {
  UseMethod("aes_eval")
}



# Method - aes_eval.dots() ------------------------------------------------


#'
aes_eval.dots <- function(maps, vars) {
  x.exists <- if (is.null(maps$x)) FALSE else TRUE
  y.exists <- if (is.null(maps$y)) FALSE else TRUE
  z.exists <- if (is.null(maps$z)) FALSE else TRUE
  xyz.exists <- list(x.exists, y.exists, z.exists)

  # Prepare the list of data frame names
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  xyz.eval <- Map(get_xyz, maps[c("x", "y", "z")], xyz.exists, list(vars))
  names(xyz.eval) <- c("x", "y", "z")

  dots.eval <- get_dots(maps$dots)

  mappings <- c(list(x = xyz.eval$x,
                     y = xyz.eval$y,
                     z = xyz.eval$z),
                dots.eval
                )

  mappings <- mappings[!vapply(mappings, is.null, logical(1))]
  class(mappings) <- "dots"

  return(mappings)
}



# Method - aes_eval.no_dots() ---------------------------------------------


#'
aes_eval.no_dots <- function(maps, vars) {
  x.exists <- if (is.null(maps$x)) FALSE else TRUE
  y.exists <- if (is.null(maps$y)) FALSE else TRUE
  z.exists <- if (is.null(maps$z)) FALSE else TRUE
  xyz.exists <- list(x.exists, y.exists, z.exists)

  # Prepare the list of data frame names
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  xyz.eval <- Map(get_xyz, maps[c("x", "y", "z")], xyz.exists, list(vars))
  names(xyz.eval) <- c("x", "y", "z")

  mappings <- c(list(x = xyz.eval$x,
                     y = xyz.eval$y,
                     z = xyz.eval$z)
                )

  mappings <- mappings[!vapply(mappings, is.null, logical(1))]
  class(mappings) <- "no_dots"

  return(mappings)
}



# Helper - aes.eval() -----------------------------------------------------


get_xyz <- function(xyz, xyz.exists, names) {

  if (xyz.exists) {

    # Strip c() wrapper or wrap in list if no c() (for is.fun()).
    xyz <- if (is_c(xyz)) xyz[-1L] else list(xyz)

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    xyz.gg2   <- rm_gg2(xyz) %R% FALSE
    xyz.dplyr <- if (isFALSE(xyz.gg2))
      seq_along(xyz) else
      seq_along(xyz)[-xyz.gg2] %R% FALSE

    # "Evaluate" both type of expression variables.
    xyz.eval <- list()
    xyz.eval[xyz.gg2]   <- if (!isFALSE(xyz.gg2))
      vapply(xyz[xyz.gg2], deparse, character(1))
    xyz.eval[xyz.dplyr] <- if (!isFALSE(xyz.dplyr))
      lapply(xyz.dplyr, function(i) messy_eval(xyz[[i]], vars, names_list))

    return(unlist(xyz.eval, use.names = FALSE))
  } else {
    return(NULL)
  }
}



# Helper - aes.eval() -----------------------------------------------------


get_dots <- function (dots, vars) {
  # Capture names (names will be lost by the dots.dplyr.eval lapply()).
  dots.names <- names(dots)
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  # Argument - modify dots to strip the outer c() is exists.
  dots <- lapply(dots, function(x) if (is_c(x)) x[-1L] else list(x))

  # Creat list to hold evaluations.
  dots.eval <- list()

  # Determine variables by expression type: ggplot2 (gg2) or dplyr.
  dots.gg2   <- lapply(dots, function(x) rm_gg2(x) %R% FALSE)
  find.dplyr <- function(x, y) {
    if (isFALSE(y)) seq_along(x)
    else seq_along(x)[-y] %R% FALSE
  }
  dots.dplyr <- Map(find.dplyr, x = dots, y = dots.gg2)

  # "Evaluate" both types of expressions.
  eval.gg2 <- function(x, y) {
    d.eval <- list()
    d.eval[rev(abs(y))] <- vapply(x[rev(y)], deparse, character(1)) %R% NULL
  }
  dots.gg2.eval   <- Map(eval.gg2, x = dots, y = dots.gg2)
  dots.dplyr.eval <- lapply(seq_along(dots.dplyr), function(i) {
    if (isFALSE(dots.dplyr[[i]])) NULL
    else {
      d.eval <- list()
      d.eval[dots.dplyr[[i]]] <- lapply(dots.dplyr[[i]], function(j) {
        messy_eval(dots[[i]][[j]], vars, names_list)
      })
      d.eval
    }
  })

  # Combine "Evaluations"
  dots.all <- lapply(seq_along(dots), function(x) {
    c(unlist(dots.gg2.eval[[x]]), unlist(dots.dplyr.eval[[x]]))
  })

  names(dots.all) <- dots.names
  return(dots.all)
}
