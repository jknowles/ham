# Human Augmented Matching

Sometimes, it's quicker to just do it yourself. 

## In Action

![](tools/readme/ham.gif)

## Code

This is a basic example which shows you how to solve a common problem:

``` r
source <- letters
key <- c(letters, paste0(letters, 2), paste0(letters, 1))
ham(source = source, choices = key)
```


