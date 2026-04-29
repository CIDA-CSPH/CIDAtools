# Create Project Directory + readme files

This function creates the standard project organization structure for
CIDA within a folder that already exists.

## Usage

``` r
create_project(
  path = getwd(),
  template = c("Admin", "Background", "Code", "DataRaw", "DataProcessed",
    "Dissemination", "Reports"),
  ProjectName = "",
  PI = "",
  analyst = "",
  datalocation = "",
  gitlocation = ""
)
```

## Arguments

- path:

  Where should they be created? Default is the working directory.

- template:

  Which subdirectories to create

- ProjectName:

  Name of project, or "" for blank

- PI:

  Name of PI and credentials, or "" for blank

- analyst:

  Name of Analyst(s), or "" for blank

- datalocation:

  Location of project on CIDA Drive, or "" for blank

- gitlocation:

  Location project on GitHub

## Value

This function creates the desired project subdirectories and readmes, as
well as a standard .gitignore file files. It will not overwrite the file
however if it does not exist. It does not return anything.

## See also

proj_setup() is the internal wrapper for this that gets called when
using the RStudio GUI to create a project
