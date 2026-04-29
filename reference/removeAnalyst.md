# Remove Default Analyst from ~/.Rprofile

This function removes the default analyst set with setAnalyst() from the
users .Rprofile. If this is the only entry in .Rprofile it will remove
the file as well.

## Usage

``` r
removeAnalyst(quiet = F)
```

## Arguments

- quiet:

  should a message indicating result be returned, if TRUE will only
  return TRUE or FALSE

## Value

Message indicating sucess or failue
