# Internal function for p-value calculation in cida_table1

Internal function for p-value calculation in cida_table1

## Usage

``` r
pvalue(
  x,
  name,
  ...,
  include_total = include_total,
  nonParametricVars = nonParametricVars
)
```

## Arguments

- x:

  the row_variable

- name:

  the variables name

- ...:

  arguments passed to S3 methods

- include_total:

  an indicator to whether the overall variable summaries are included in
  the table. Used to ensure that p-values are only computed across
  grouped variables.

- nonParametricVars:

  a vector of the names of the variables that should use non-parametric
  hypothesis testing
