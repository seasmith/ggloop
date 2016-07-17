
# WORKS -------------------------------------------------------------------
gg1.1 <- ggloop(mtcars,
              aes_loop(mpg:hp, mpg:cyl, color = am:carb),
              TRUE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg1.2 <- ggloop(mtcars,
              aes_loop(mpg:disp, mpg:hp, color = am:carb),
              FALSE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg2.1 <- ggloop(mtcars,
              aes_loop(mpg:hp, mpg:hp, color = am:carb, size = vs:am),
              TRUE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg2.2 <- ggloop(mtcars,
              aes_loop(mpg:hp, mpg:hp, color = am:carb, size = vs:am),
              TRUE,
              TRUE)


# FAILS -------------------------------------------------------------------
gg3.1 <- ggloop(mtcars,
                aes_loop(mpg, cyl:hp),
                TRUE,
                FALSE)


# FAILS -------------------------------------------------------------------
gg3.2 <- ggloop(mtcars,
                aes_loop(mpg:hp, c(1,3,5,7)),
                TRUE,
                TRUE)


# FAILS -------------------------------------------------------------------
gg4.1 <- ggloop(mtcars,
                aes_loop(mpg:hp, mpg:hp),
                FALSE,
                FALSE)


# FAILS -------------------------------------------------------------------
gg4.2 <- ggloop(mtcars,
                aes_loop(mpg:hp, mpg:hp),
                FALSE,
                TRUE)
