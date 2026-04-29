# Pretty p-values

This function helps print p-values in RMD output

## Usage

``` r
pvalr(pvals, sig.limit = 0.001, digits = 3, html = FALSE, equal_sign = "")
```

## Arguments

- pvals:

  a vector of numeric p-values

- sig.limit:

  A lower threshold below which to print "\<sig.limit"

- digits:

  how many digits to print?

- html:

  uses the HTML symbol instead of normal \< symbol

- equal_sign:

  character value to append to front of p-value

## Value

A character vector of "pretty" p values
