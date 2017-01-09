library(ggloop)
library(ggplot2)


# ggloop example ----------------------------------------------------------

# The ggloop() example failed on the dev branch circa January 2017.

context("List names")

# dev branch #1
# y-aesthetic is mapping to the color (dots) aesthetic.
test_that("y aesthetic is mapping to the color aesthetic", {

plots2 <- ggloop(data = mtcars, mappings = aes_loop(x = c(disp, hp, wt),
                                                    y = mpg,
                                                    color = factor(cyl)))

expected_names <- c("color.factor(cyl)")
expect_match(names(plots2), expected_names, fixed = TRUE)
})
