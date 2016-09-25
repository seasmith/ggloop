# All "dots" should have the exact same xy's. Run test to determine if xy's are
# all the same among "dots".

# xydot -----------------------------------------------------------------


xydot.TF <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         TRUE,
                         FALSE)

xydot.TT <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         TRUE,
                         TRUE)

xydot.FF <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:disp, mpg:hp, color = am:carb),
                         FALSE,
                         FALSE)

xydot.NAT <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:hp, mpg:cyl, color = am:carb),
                         NA,
                         TRUE)

xydot.NAF <- ggloop::ggloop(mtcars,
                         ggloop::aes_loop(mpg:disp, mpg:hp, color = am:carb),
                         NA,
                         FALSE)

# xydots ----------------------------------------------------------------


xydots.TF <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          TRUE,
                          FALSE)

xydots.TT <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          TRUE,
                          TRUE)

xydots.FF <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          FALSE,
                          FALSE)

xydots.NAT <- ggloop::ggloop(mtcars,
                          ggloop::aes_loop(x = mpg:hp,
                                   y = mpg:hp,
                                   color = am:carb,
                                   size = vs:am),
                          NA,
                          TRUE)

xydots.NAF <- ggloop::ggloop(mtcars,
                           ggloop::aes_loop(x = mpg:hp,
                                    y = mpg:hp,
                                    color = am:carb,
                                    size = vs:am),
                           NA,
                           FALSE)


# xy ---------------------------------------------------------------------


xyTF <- ggloop::ggloop(mtcars,
                ggloop::aes_loop(x = mpg,
                         y = cyl:hp),
                TRUE,
                FALSE)

xyTT <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     TRUE,
                     TRUE)

xyFF <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     FALSE,
                     FALSE)

xyNAT <- ggloop::ggloop(mtcars,
                     ggloop::aes_loop(x = mpg,
                              y = cyl:hp),
                     NA,
                     TRUE)

xyNAF <- ggloop::ggloop(mtcars,
                      ggloop::aes_loop(x = mpg,
                               y = cyl:hp),
                      NA,
                      FALSE)
