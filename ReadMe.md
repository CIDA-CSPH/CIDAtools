# CIDAtools

The primary purpose of this package is to house RProject Templates that automatically setup folder structures, ReadMe, and .git according to the preferred workflow at CIDA.

<img src="inst/figures/CIDAtoolshex.png" alt="CIDAtools" width="259" height="300"/>

`devtools::install_github('CIDA-CSPH/CIDAtools')`

To create a new project folder structure, navigate to the main project directory, and run: 

```
createProject()
```

Feel free to set the project name, PI, and analyst via the `ProjectName`, `PI`, or `analyst`
arguments respectively, and see `?createProject` for more details. If any of these are
specified, they are stored as text in a hidden subdirectory called .ProjData, and can
be called in subsequent R code anywhere in the project using `ProjectName()`, `ProjectPI()`, and
`ProjectAnalyst()`. 

They can also be used in snippets (if you have a header snippet already in your Rstudio options):

Example of header snippet using Project Data:
```
snippet header
	###########################################
	# Project: `r CIDAtools::ProjectName()`
	# Author: `r CIDAtools::ProjectAnalyst()`
	# Date: `r paste(format(Sys.Date(), '%m/%d/%Y'))`
	# #########################################
```

This package contains numerous templates and tools to make life at CIDA (part 
of the Colorado School of Public Health) happier and more efficient!

This package also contains functions for reading excel files with colour columns
and the Table1 function. 

Please feel free to file an issue request if you encounter errors or would like 
to request new features.  

## News/Notes

Currently tibbles do not work with `Table1()`. To use `Table1()` with a tibble please use `as.data.frame()`. A new function 
(`desc_table()`)
utilizing tidy principles and compatible with the tidyverse is under construction. A preliminary version (lacking some of the 
ultimate functionality) is available/working. 
If you would like to use it while it is in development it is under development in the desc_table branch. 
The commit messages/code comments 
contain details on where in the process I am at. 
