# ggloop 0.2.0
## Changes

* New class `gglist` assigned to returned objects from `ggloop()`. For lists with two levels, those with `dot` aesthetics (non-x and non-y), both levels will receive the `gglist` class.

```{r}
g <- ggloop(mtcars, aes_loop(mpg, wt, color = factor(cyl)))
g$`color.factor(cyl)` %>% class

#> "gglist"

g$`color.factor(cyl)` %>% class

#> "gglist"
```

* Method `+.gglist` added to allow more convenient, `ggplot2`-like building of plots (i.e. `a_list_of_plots + geom_point()`).
* `%L+%` will now be deprecated at some point in the future (probably next minor/patch release) due to the introduction of the `+.gglist` operator.
* `[.gglist` will now allow a `gglist` object to retain its class when subsetted: 

```{r}
g <- ggloop(mtcars, aes_loop(mpg, wt, color = c(factor(cyl), factor(gear))))
g[1:2] %>% class()

#> "gglist"

g$`color.factor(cyl)`[1:2] %>% class

#> "gglist"
```
