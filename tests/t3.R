
# WORKS -------------------------------------------------------------------
gg1.1 <- ggloop(mtcars,
              aes_loop3(mpg:hp, mpg:hp, color = am:carb),
              TRUE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg1.2 <- ggloop(mtcars,
              aes_loop3(mpg:hp, mpg:hp, color = am:carb),
              FALSE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg2.1 <- ggloop(mtcars,
              aes_loop3(mpg:hp, mpg:hp, color = am:carb, size = vs:am),
              TRUE,
              FALSE)


# WORKS -------------------------------------------------------------------
gg2.2 <- ggloop(mtcars,
              aes_loop3(mpg:hp, mpg:hp, color = am:carb, size = vs:am),
              TRUE,
              TRUE)


# FAILS -------------------------------------------------------------------
gg3.1 <- ggloop(mtcars,
                aes_loop3(mpg:hp, mpg:hp),
                TRUE,
                FALSE)


# FAILS -------------------------------------------------------------------
gg3.2 <- ggloop(mtcars,
                aes_loop3(mpg:hp, mpg:hp),
                TRUE,
                TRUE)


# FAILS -------------------------------------------------------------------
gg4.1 <- ggloop(mtcars,
                aes_loop3(mpg:hp, mpg:hp),
                FALSE,
                FALSE)


# FAILS -------------------------------------------------------------------
gg4.2 <- ggloop(mtcars,
                aes_loop3(mpg:hp, mpg:hp),
                FALSE,
                TRUE)
