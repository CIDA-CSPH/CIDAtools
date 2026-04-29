# Get Project data location on CIDA Drive

Get Project data location on CIDA Drive

## Usage

``` r
ProjectLocation(path = "")
```

## Arguments

- path:

  (optional) a relative path to a particular place in the project

## Value

full (absolute) file path including the project location on CIDA drive

## Examples

``` r
# Read data from current project
if (FALSE) { # \dontrun{
df <- read.csv(ProjectLocation("DataRaw/my_proj_data.csv"))
} # }
```
