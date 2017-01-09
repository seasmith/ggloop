proto_aes_loop <- function(x, y, z, ...) {
  if (!missing(x)) x <- substitute(x) else x <- NULL
  if (!missing(y)) y <- substitute(y) else y <- NULL
  if (!missing(z)) z <- substitute(z) else z <- NULL
  dots <- as.list(substitute(list(...)))[-1L]

  if (all(is.null(x), is.null(y), is.null(z)))
    stop("You must supply at least one x, y, or z aesthetic")

  # Structure - build and assign class to pass to generics
  maps <- list(x = x, y = y, z = z, dots = dots)
  class(maps) <- if (is.null(maps$dots[1][[1]])) "no_dots" else "dots"

  return(maps)
}

proto_aes_eval <- function(aesth) {
  x.exists <- if (is.null(aesth$x)) FALSE else TRUE
  y.exists <- if (is.null(aesth$y)) FALSE else TRUE
  z.exists <- if (is.null(aesth$z)) FALSE else TRUE
  xyz.exists <- list(x.exists, y.exists, z.exists)

  return(xyz.exists)
}


ops <- c("/", "\\+", "-", "\\*", "\\^")

is.op <- function(lst) {
  has.ops <- sapply(ops, function(expr) grep(expr, lst))
  has.ops <- unlist(has.ops)
  names(has.ops) <- NULL
  unique(has.ops)
}

f <- function(x){
  vect <- c(T, F, NA)
  print(match.arg(as.character(x), as.character(vect)))
}

plots2 <- ggloop(data = mtcars, mappings = aes_loop(x = c(disp, hp, wt),
                                                    y = mpg,
                                                    color = factor(cyl)))
