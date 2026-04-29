# Read in xlsx with fill colour

Reads in the fill colour of excel workbooks. Creates a data frame for
each sheet in a list if mutliple sheets are requested. Creates a colour
column for each colour column specified.

## Usage

``` r
read_xlsx_color(file, colorColumns, sheet = NULL, header = T)
```

## Arguments

- file:

  the path to the file you intend to read. Can be an xls or xlsx format.

- colorColumns:

  column numbers for which you want to read the colour, for multiple
  sheets pass a list for each sheet (see details). For no colour columns
  pass zero.

- sheet:

  NULL(default) for all sheets otherwise a vector of sheet numbers or
  names to read.

- header:

  should the 1st row be read in as a header? defaults to T.

## Value

A data frame for one sheet or a list of data frames for multiple sheets

## Details

For `colourColumns` pass a list of numeric vectors for each sheet. For
example for 2 sheets `colourColumns = list(c(1,2), c(3))` for columns 1
and 2 in the first sheet and 3 in the second. If the list of
`colourColumns` is shorter than the sheets the remaining sheets will be
assumed to have no colour columns. If the list of colour columns is
longer only the first n elements will be used where n is the number of
sheets with a warning.
