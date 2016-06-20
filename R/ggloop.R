

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
  mappings <- aes_inputs2(data, x, y, ...)


  # remap TRUE/FALSE/NA xy value pairs
  if(is.na(remap_xy))
    remap_xy_NA(mappings) else{
      if(remap_xy)
        remap_xy_TRUE(mappings) else
          if(!remap_xy)
            remap_xy_FALSE(mappings)
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






