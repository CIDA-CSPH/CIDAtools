# Round and don't drop trailing zeros

Shorter wrapper for format(x, digits = n, nsmall = n)

## Usage

``` r
Round(x, n)
```

## Arguments

- x:

  numeric to be formatted

- n:

  number of digits for nsmall

## Value

a character vector of same length of x converted

## Details

should not be used unless digits after a decimal are needed. Note for
numbers with leading zeros (ie. 0.0349) you will get one more decimal
place than n. (ie. `Round(O.0349, 2)` will return `0.035`)
