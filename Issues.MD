### To Do's
* Should aes's that have a y value but no x value be coerced to x value?

```{r}
# old
y -> mpg
colour -> gear

# new
x -> mpg
colour -> gear
```
* Call $x and $y position without having to use c(2, 4)
    * Won't work if $x is missing because then 4 will be $is.dots
* Change $is.x, $is.y, $is.dots to hold functions that can determine:
    * If x, y, and dots exists
    * Position of x, y, and dots
    * Length of x, y, and dots
* Work on listifyr:
    * Determine list element position by name
    * Better str() for list
