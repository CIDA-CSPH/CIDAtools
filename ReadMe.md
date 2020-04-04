
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

**Templates** are useful for creating new documents with the correct CIDA *formatting*.

`CIDAtools` includes templates that can make it easy to create a new CIDA report. 
To use this functionality in Rstudio, first ensure the package is installed, then: 

1) Click on "New R Markdown" 
2) In the pop-up, select "From template"
3) You should see CIDA report templates to chooose from. 

Alternatively, source code for templates is available in inst/rmarkdown/templates/. 


## Outlines for CIDA reports

**Outlines** are intended to provide an idea of the *content* necessary within each type of CIDA report. 

`CIDAtools` includes outlines for the following reports in inst/outlines/: 

- CIDA Analysis Plan [[download]](https://github.com/CIDA-CSPH/CIDAtools/raw/master/inst/outlines/CIDA%20Analysis%20Plan%20Outline.docx)
- CIDA Comprehensive Report [[download]](https://github.com/CIDA-CSPH/CIDAtools/raw/master/inst/outlines/CIDA%20Comprehensive%20Report%20Outline.docx)
- CIDA Exploratory Report [[download]](https://github.com/CIDA-CSPH/CIDAtools/raw/master/inst/outlines/CIDA%20Exploratory%20Report%20Outline.docx)
- CIDA Omics Study Report [[download]](https://github.com/CIDA-CSPH/CIDAtools/raw/master/inst/outlines/CIDA%20Omics%20Outline.docx)
- CIDA Study Design [[download]](https://github.com/CIDA-CSPH/CIDAtools/raw/master/inst/outlines/CIDA%20Study%20Design%20Outline.docx)

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

