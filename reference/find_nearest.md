# Find the nearest observation to another observation

This function finds the nearest y to every x. Y's may be duplicated.

## Usage

``` r
find_nearest(
  x,
  y,
  direction = c("both", "ascending", "descending"),
  returnIndex = FALSE
)

getlower(x, y, upper = FALSE)
```

## Arguments

- x:

  first value

- y:

  second value

- direction:

  default = both, ascending for only y matches before x and descending
  for only y matches after x.

- returnIndex:

  should an index of the mathced y values be returned instead of the
  matched list.

- upper:

  upper value?

## Value

a list of length 2 with a y matched to every x, note if direction =
'ascending' or 'descending', NAs will be returned for x values with no y
values before or after, respectively. OR an index of matched y values if
`returnIndex = TRUE`.

indexes of y for each x

## Functions

- `getlower()`: function for finding lower(upper) value

## References

This function borrowed heavily from this stack exchange post:
https://stats.stackexchange.com/questions/161379/quickly-finding-nearest-time-observation
