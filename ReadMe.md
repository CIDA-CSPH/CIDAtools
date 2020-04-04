
<img src="inst/figures/CIDAtoolshex.png" alt="CIDAtools" width="259" height="300"/>

This package contains numerous templates and tools to make life at CIDA (part 
of the Colorado School of Public Health) happier and more efficient!

To install `CIDAtools` on your local machine:

```
# install.packages("devtools")
devtools::install_github('CIDA-CSPH/CIDAtools')
```

## Creating a new project

To create a new project folder structure, navigate to the main project directory, and run: 

```
CreateProject()
```

Feel free to set the project name, PI, and analyst via the `ProjectName`, `PI`, or `analyst`
arguments respectively, and see `?CreateProject` for more details. If any of these are
specified, they are stored as text in a hidden subdirectory called .ProjData, and can
be called in subsequent R code anywhere in the project using `ProjectName()`, `ProjectPI()`, and
`ProjectAnalyst()`. 

## Creating a new CIDA report

Templates are available with CIDAtools that can make it easy to create a new CIDA report. 
To use this functionality in Rstudio, first ensure the package is installed, then: 

1) Click on "New R Markdown" 
2) In the pop-up, select "From template"
3) You should see CIDA report templates to chooose from. 

Alternatively, source code for templates is available in inst/rmarkdown/templates/. 

## Outlines for CIDA reports

Templates are useful for creating new documents with the correct CIDA formatting. 
Outlines, on the other hand, are intended to provide an idea of what should be included in 
each type of report. 

CIDAtools includes outlines for the following reports in inst/outlines/: 
- CIDA Analysis Plan 
- CIDA Comprehensive Report
- CIDA Exploratory Report
- CIDA Omics Study Report
- CIDA Study Design 

## Using snippets

CIDA functions can be used in snippets (if you have a header snippet already in your Rstudio options):

Example of a header snippet using Project Data:
```
snippet header
	###########################################
	# Project: `r CIDAtools::ProjectName()`
	# Author: `r CIDAtools::ProjectAnalyst()`
	# Date: `r paste(format(Sys.Date(), '%m/%d/%Y'))`
	# #########################################
```

## Other functionality 

This package also contains functions for reading excel files with color columns
and the Table1 function. 

Please feel free to file an issue request if you encounter errors or would like 
to request new features.  

