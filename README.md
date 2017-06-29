# Human Augmented Matching

Sometimes, it's quicker to just do it yourself. 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
source <- letters
key <- c(letters, paste0(letters, 2), paste0(letters, 1))
ham(source = source, key = key)
```
