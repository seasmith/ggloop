# Represents the first steps in aes_loop().
expr_maker <- function(x, y, ...) {
  if(!missing(x)) x <- substitute(x) else x <- NULL
  if(!missing(y)) y <- substitute(y) else y <- NULL
  dots <- as.list(substitute(list(...)))[-1L]
  return(list(x = x, y = y, dots = dots))
}

e <- expr_maker(x = c(disp, hp, wt),
                y = c(mpg),
                color = factor(cyl))

e$x <- if (is_c(e$x)) e$x[-1L] else list(e$x)
e$y <- if (is_c(e$y)) e$y[-1L] else list(e$y)

x.gg2 <- rm_gg2(e$x) %R% FALSE

x.dplyr <- if (isFALSE(x.gg2)) seq_along(e$x) else {
  seq_along(e$x)[-x.gg2] %R% FALSE
}

vars <- names(mtcars)
names_list <- stats::setNames(as.list(seq_along(vars)), vars)

x.eval <- list()
x.eval[x.gg2]   <- if (!isFALSE(x.gg2)) vapply(e$x[x.gg2], deparse, character(1))
x.eval[x.dplyr] <- if (!isFALSE(x.dplyr)) {
  lapply(x.dplyr, function(i) messy_eval(e$x[[i]], vars, names_list))
}

x.res <- unlist(x.eval, use.names = FALSE)


# Test get_xy() -----------------------------------------------------------

e <- expr_maker(x = c(mpg:cyl, 5:6, qsec, 8, factor(cyl), mpg/wt),
                y = c(mpg:cyl, 5:6, qsec, 8, factor(cyl), mpg/wt),
                color = c(am:carb, 1:2, qsec, 3,
                          factor(cyl), mpg/wt))

x.exists <- if (is.null(e$x)) FALSE else TRUE
y.exists <- if (is.null(e$y)) FALSE else TRUE
xy.exists <- list(x.exists, y.exists)

get_xy <- function(aes, aes.exists, names) {

  if (aes.exists) {

    # Strip c() wrapper or wrap in list if no c() exists (for is.fun()).
    aes <- if (is_c(aes)) aes[-1L] else list(aes)

    # Determine variables by expression type: ggplot2 (gg2) or dplyr.
    aes.gg2   <- rm_gg2(aes) %R% FALSE
    aes.dplyr <- if (isFALSE(aes.gg2)) seq_along(aes) else {
      seq_along(aes)[-aes.gg2] %R% FALSE
    }

    # "Evaluate" both type of expression variables.
    aes.eval <- list()
    aes.eval[aes.gg2]   <- if (!isFALSE(aes.gg2)) vapply(aes[aes.gg2], deparse, character(1))
    aes.eval[aes.dplyr] <- if (!isFALSE(aes.dplyr)) {
      lapply(aes.dplyr, function(i) messy_eval(aes[[i]], vars, names_list))
    }

    return(unlist(aes.eval, use.names = FALSE))
  } else {
    return(NULL)
  }
}

xy.eval <- Map(get_xy, list(e$x, e$y), xy.exists, list(vars))
names(xy.eval) <- c("x", "y")



# Test dots ---------------------------------------------------------------

attach(e)

if (length(dots)) {
  # Capture names (names will be lost by the dots.dplyr.eval lapply()).
  dots.names <- names(dots)

  # Strip c().
  dots <- lapply(dots, function(x) if (is_c(x)) x[-1L] else list(x))

  # Creat list to hold evaluations.
  dots.eval <- list()

  # Determine variables by expression type: ggplot2 (gg2) or dplyr.
  dots.gg2   <- lapply(dots, function(x) rm_gg2(x) %R% FALSE)
  find_dplyr <- function(x, y) {
    if (isFALSE(y)) seq_along(x)
    else seq_along(x)[-y] %R% FALSE
  }
  dots.dplyr <- Map(find_dplyr, x = dots, y = dots.gg2)

  # "Evaluate" both types of expressions.
  eval_gg2 <- function(x, y) {
    d.eval <- list()
    d.eval[rev(abs(y))] <- vapply(x[rev(y)], deparse, character(1)) %R% NULL
  }
  dots.gg2.eval   <- Map(eval_gg2, x = dots, y = dots.gg2)
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

  # Combine dots "Evaluations"
  dots.eval <- lapply(seq_along(dots), function(x) {
    c(unlist(dots.gg2.eval[[x]]), unlist(dots.dplyr.eval[[x]]))
  })

  names(dots.eval) <- dots.names
  is.dots <- TRUE
} else {
  dots.eval <- NULL
  is.dots   <- FALSE
}



# Test remap_xy -----------------------------------------------------------


