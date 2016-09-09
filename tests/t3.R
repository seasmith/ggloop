

# x.y.dot -----------------------------------------------------------------


gg.x.y.dot.T.F <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         TRUE,
                         FALSE)

gg.x.y.dot.T.T <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         TRUE,
                         TRUE)

gg.x.y.dot.F.F <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:disp, mpg:hp, color = am:carb),
                         FALSE,
                         FALSE)

gg.x.y.dot.NA.T <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         NA,
                         TRUE)

gg.x.y.dot.NA.F <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:disp, mpg:hp, color = am:carb),
                         NA,
                         FALSE)

# x.y.dots ----------------------------------------------------------------


gg.x.y.dots.T.F <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          TRUE,
                          FALSE)

gg.x.y.dots.T.T <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          TRUE,
                          TRUE)

gg.x.y.dots.F.F <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          FALSE,
                          FALSE)

gg.x.y.dots.NA.T <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          NA,
                          TRUE)

gg.x.y.dots.NA.F <- ggloop::ggloop(mtcars,
                           ggloop::aes_loop(x = mpg:hp,
                                    y = mpg:hp,
                                    color = am:carb,
                                    size = vs:am),
                           NA,
                           FALSE)


# x.y ---------------------------------------------------------------------


gg.x.y.T.F <- ggloop::ggloop(mtcars,
                ggloop::aes_loop(x = mpg,
                         y = cyl:hp),
                TRUE,
                FALSE)

gg.x.y.T.T <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     TRUE,
                     TRUE)

gg.x.y.F.F <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     FALSE,
                     FALSE)

gg.x.y.NA.T <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     NA,
                     TRUE)

gg.x.y.NA.F <- ggloop::ggloop(mtcars,
                      ggloop::aes_loop(x = mpg,
                               y = cyl:hp),
                      NA,
                      FALSE)
