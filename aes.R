mappings <- aes_inputs(data = mtcars,
                       x = mpg:hp,
                       y = c(disp, wt),
                       color = gear:carb,
                       lty = am)
