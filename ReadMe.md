# OPPtools

<!--<a href='https://cida-csph.github.io/CIDAtools'><img src='inst/figures/CIDAtoolshex.png' align="right" height="139" /></a>

[![R-CMD-check](https://github.com/CIDA-CSPH/CIDAtools/workflows/R-CMD-check/badge.svg)](https://github.com/CIDA-CSPH/CIDAtools/actions)
-->

## Overview

This package is a fork of [CIDAtools](https://github.com/CIDA-CSPH/CIDAtools), and allows you to setup a standardized .Rproj directory with the following preset subdirectories:

1. 01_Raw_data
2. 02_Analysis
3. 03_Working_files
4. 04_Processed
5. 05_Admin

In addition, a README.md file will be generated for all newly created directories to describe which files are contained within.

To install `OPPtools` on your local machine:

```
# install.packages("devtools")
devtools::install_github('popovs/OPPtools')
```

## Creating a new project

After `OPPtools` is installed, a new OPP project can be created using the
Rstudio GUI (File>New Project>New Directory>New OPP Project).

Or, to create a new project folder structure from the R console, 
navigate to the main project directory, and run: 

```
CreateProject()
```

Feel free to set the project name, PI, and analyst via the `ProjectName`, `PI`, or `analyst`
arguments respectively, and see `?CreateProject` for more details. If any of these are
specified, they are stored as text in a hidden subdirectory called .ProjData, and can
be called in subsequent R code anywhere in the project using `ProjectName()`, `ProjectPI()`, and
`ProjectAnalyst()`. 

## Documentation

The package website of the original project, `CIDAtools`, is available [here](https://cida-csph.github.io/CIDAtools). 
Further documentation on the original `CIDAtools` functions can be found there. 

## Using snippets

`OPPtools` functions can be used in snippets (if you have a header snippet already in your Rstudio options):

Example of a header snippet using Project Data:
```
snippet header
	###########################################
	# Project: `r OPPtools::ProjectName()`
	# Author: `r OPPtools::ProjectAnalyst()`
	# Date: `r paste(format(Sys.Date(), '%m/%d/%Y'))`
	# #########################################
```

