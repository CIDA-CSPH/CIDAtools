# List tables with the same columns

This function will print a group of tables together in a decently pretty
way. May need a bit of finagling.

## Usage

``` r
list_kables(tabs, bo = c("striped", "condensed"), ...)
```

## Arguments

- tabs:

  A list of data frames or tibbles with the same number and names of
  columns

- bo:

  boostrap options to pass to kableExtra

- ...:

  other options to be passed to kable, such as caption, align, etc.

## Value

HTML or Latex output for pretty tables
