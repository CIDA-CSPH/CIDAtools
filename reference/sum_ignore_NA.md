# Sum ignoring NAs

Will sum values returning NA only if all values are NA, otherise will
ignore

## Usage

``` r
sum_ignore_NA(...)
```

## Arguments

- ...:

  numbers or vectors to be summed. Must be type logical or numeric.

## Value

a numeric vector of the same length as the arguments

## Details

this function will provide vectorized sums with NAs ignored unless only
NAs are present

## Examples

``` r
# ignores NA
sum_ignore_NA(2, 3, NA)
#> [1] 5
# returns NA if all values are NA
sum_ignore_NA(NA, NA, NA)
#> [1] NA

# returns vectorized sums

x <- c(1, 2, NA)
y <- c(1:3)
sum_xy <- sum_ignore_NA(x, y)
data.frame(x, y, sum_xy)
#>    x y sum_xy
#> 1  1 1      2
#> 2  2 2      4
#> 3 NA 3      3

x <- c(1, 2, NA)
y <- c(1, 2, NA)
sum_xy <- sum_ignore_NA(x, y)
data.frame(x, y, sum_xy)
#>    x  y sum_xy
#> 1  1  1      2
#> 2  2  2      4
#> 3 NA NA     NA
```
