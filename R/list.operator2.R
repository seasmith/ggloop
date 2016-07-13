`%L+%` <- function(lhs, rhs){
  # 0. rhs = ggproto
  rhs.test <- ggplot2::is.ggproto(rhs)

  # 1. lhs = ggplot
  test_ggplot <- ggplot2::is.ggplot(lhs)

  # 2. lhs = list(ggplot)
  test_list <- is.list(lhs)
  test_list.ggplot <- all(sapply(lhs, ggplot2::is.ggplot))

  # 3. lhs = list(list(ggplot))
  test_list.list <- all(sapply(lhs, is.list))
  test_list.list.ggplot <- all(sapply(lhs, function(x){
    sapply(x, ggplot2::is.ggplot)
  }))

  lhs.test <- c(first = test_ggplot,
                second = all(test_list, test_list.ggplot),
                third = all(test_list.list, test_list.list.ggplot))

# MUST ADD CODE TO HANDLE NAMES!
  # First case
  first <- function() lhs + rhs

  # Second case
  second <- function(){
    attr.1 <- attributes(lhs)

    result <- lapply(seq_along(lhs), function(x){
      lhs[[x]] + rhs
  })

    attributes(result) <- attr.1

  # Third case
  third <- function(){
    attr.1 <- attributes(lhs)

    result.1 <- lapply(seq_along(lhs), function(x){

      attr.2 <- attributes(lhs[[x]])

      result.2 <- lapply(seq_along(x), function(y){
        lhs[[x]][[y]] + rhs
        })
      attributes(result.2) <- attr.2
      })

    attributes(result.1) <- attr.1

    return(result.1)
  }

  fun.list <- list(first,
                   second,
                   third)

  if(rhs.test && sum(lhs.test)){
    fun.list[[which(lhs.test)]]()
  } else{
    stop("The right-hand side or left-hand side are not of proper class")
  }

}
