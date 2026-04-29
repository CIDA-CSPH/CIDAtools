# Get CIDA drive path

This function attempts to get the proper path for the CIDA drive either
on Windows or Mac.

## Usage

``` r
CIDA_drive_path(file = "")
```

## Arguments

- file:

  (optional) Path to subdirectory/file within CIDA drive

## Value

Full (absolute) file path of CIDA drive

## Examples

``` r
# Read data from P1234PIname project
if (FALSE) { # \dontrun{
df <- read.csv(CIDA_drive_path("BRANCHES/Pulmonary/P1234PIname/DataRaw/data.csv"))
} # }
```
