
# isFALSE() ---------------------------------------------------------------
#
# This is an abbreviation of identical(FALSE, x) to go along with isTRUE()

isFALSE <- function(x) identical(FALSE, x)


# ggloop() ----------------------------------------------------------------


ggloop <- function(data,
                   mapping = aes_loop(),
                   remap_xy = TRUE,
                   remap_dots = NA,
                   environment = parent.frame()){

}


# aes_loop() --------------------------------------------------------------


aes_loop <- function(data, x, y, remap_xy, remap_dots, ...){
  # place mapping argument values in a list
  mappings <- aes_assign(data, x, y, ...)


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(remap_xy)) remap_xy_NA(mappings) else{
      if(remap_xy) remap_xy_TRUE(mappings) else{
          if(!remap_xy) remap_xy_FALSE(mappings)
      }
    }

  # remap TRUE/FALSE dots
  if(is.na(remap_dots))
    remap_dots_NA(mappings) else{
      if(remap_dots)
        remap_dots_TRUE(mappings) else
          if(!remap_dots)
            remap_dots_FALSE(mappings)
    }


}


# aes_loop2() -------------------------------------------------------------


aes_loop2 <- function(data, x, y, remap_xy, remap_dots, ...){
  # place mapping argument values in a list
  mappings <- aes_inputs2(data, x, y, ...)

    # Logic vector indicating value of remap_xy argument
    logic_xy <- c(isTRUE(remap_xy),
                  isFALSE(remap_xy),
                  is.na(remap_xy))
    # Logic vector of the three possible functions to run
    FUN_xy <- list(if_TRUE  = remap_xy_TRUE,
                   if_FALSE = remap_xy_FALSE,
                   if_NA    = remap_xy_NA)
  # Subset FUN_xy to run the appropriate function
  mappings <- FUN_xy[[which(logic_xy)]](mappings)


    # Logic vector indicating value of remap_xy argument
    logic_dots <- c(isTRUE(remap_dots),
                    isFALSE(remap_dots))
    # Logic vector of the two possible functions to run
    FUN_dots <- list(if_TRUE = remap_dots_TRUE,
                     if_FALSE = remap_dots_FALSE)
  # Subset FUN_dots to run the appropriate function
  mappings <- FUN_dots[[which(logic_dots)]](mappings)
}
