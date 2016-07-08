function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
{
  nargs <- length(args <- list(...))
  if (!nargs)                                       # if no args
    return(as.data.frame(list()))

  if (nargs == 1L && is.list(a1 <- args[[1L]]))     # if only a list
    nargs <- length(args <- a1)

  if (nargs == 0L)                                  # if arg is blank
    return(as.data.frame(list()))

  cargs <- vector("list", nargs)        # create vector = nargs.size
  iArgs <- seq_len(nargs)               # create iterator
  nmc <- paste0("Var", iArgs)           # default headings (Var1, Var2, etc)
  nm <- names(args)                     # args names (for headings)

  if (is.null(nm))                    # use default headings if no names
    nm <- nmc
  else if (any(ng0 <- nzchar(nm)))    # set names if names exist
    nmc[ng0] <- nm[ng0]
  names(cargs) <- nmc
  rep.fac <- 1L
  d <- lengths(args)
  if (KEEP.OUT.ATTRS) {
    dn <- vector("list", nargs)
    names(dn) <- nmc
  }
  orep <- prod(d)                     # set number of combinations among vectors
  if (orep == 0L) {                   # set cargs if one is of length 0
    for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
  }
  else {
    for (i in iArgs) {
      x <- args[[i]]
      if (KEEP.OUT.ATTRS)
        dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x))
          format(x)
          else x)
      nx <- length(x)
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,
                                                  nx)), orep)]
      if (stringsAsFactors && !is.factor(x) && is.character(x))
        x <- factor(x, levels = unique(x))
      cargs[[i]] <- x
      rep.fac <- rep.fac * nx
    }
  }
  if (KEEP.OUT.ATTRS)
    attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
  rn <- .set_row_names(as.integer(prod(d)))
  structure(cargs, class = "data.frame", row.names = rn)
}




