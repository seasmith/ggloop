# ggloop

> Create ggplot2 plots in a loop.

`ggloop` allows the user to use both `dplyr`-like and `ggplot2`-like syntax to call multiple aesthetic pairs. This has the potential to save the users on the amount of code within their projects or session.

## How to use `ggloop`

`ggloop` has three main functions: 
* `ggloop(data, mappings = aes_loop(), remap_xy = TRUE, remap_dots = FALSE, ..., environment = parent.frame() )` = Meant to mimick `ggplot()`'s arguments with addition remap arguments to control the remapping behavior of the mappings. There are three returned values:
	* A single `ggplot` object = Created by `x`, `y`, and `...` arguments of length one or less.
	* A list of `ggplot` objects = Created when there is no `...` argument in `aes_loop()`
	* A nest list (a list of a list) of `ggplot` objects = Created when a `...` argument is supplied. `...` names sit on the top-level of the nested list (they divide the list into however many parts based on the number of such arguments and the remapping behavior). `x` and `y` sit at the bottom-level of the nested list
* `aes_loop()` = meant to mimick `aes()`; can accept one or more arguments (a vector of arguments) with `dplyr`-like and `ggplot2`-like syntax; both syntax styles can be combined for one argument using the `c()` as a wrapper and only `c()`.
	* `dplyr`-like = `mpg:hp`, `1`, `5:9`, `cyl`, etc
	* `ggplot2`-like = `factor(cyl)`, `gear + cyl`, etc
* `%L+%` = magrittr-like (rip-off) to accomodate the addition of geoms, stats, etc to any of the returned values of `ggloop()`
