#'Create Project Directory + readme files
#'
#'This function creates the standard project organization structure for CIDA
#'within a folder that already exists.
#'
#'@param template Which subdirectories to create
#'@param path Where should they be created? Default is the working directory.
#'@param ProjectName Name of project, or "" for blank
#'@param PI Name of PI and credentials, or "" for blank
#'@param analyst Name of Analyst(s), or "" for blank
#'
#'@return This function creates the desired project subdirectories and readmes,
#'  as well as a standard .gitignore file files. It will not overwrite the file
#'  however if it does not exist. It does not return anything.
#'@keywords project createproject
#'
#'@export
CreateProject <- function(
  template = c('Admin', 'Background', 'Code', 'DataRaw',
               'DataProcessed', 'Dissemination', 'Reports'),
  path = getwd(), ProjectName = "", PI = "", analyst = "") {

  # has meta been provided?
  meta <- !all(c(ProjectName, PI, analyst) %in% "")

  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # Overall readme

  readme <- c(paste0("**Project Name**:", ProjectName, "  "),
              paste0("**PI**:", PI, "  "),
              paste0("**Analyst**:", analyst, "  "),
              "",
              "Details about the folders:",
              '',
              "File | Description",
              "---|----------------------------------------------------------",
              paste("Admin | contains the scope of work and other",
                    "administrative documents"),
              paste("Background | contains the background information for",
                    "the analysis"),
              "Code | contains all R scripts for this project",
              "DataRaw | contain all raw data provided by investigators",
              "DataProcessed | contains the processed data used for analysis",
              paste("Dissemination | contains any materials produced for",
                    "dissemination, ie. Abstracts, Posters, Papers"),
              "Reports | contains all output, rmarkdown files and report")


  # write to readme file
  writeLines(paste0(readme, collapse = '\n'),
             con = file.path(path, "ReadMe.md"))

  # Create subdirectory readmes
  CreateReadMe(template = template, path = path)

  # Add .ProjData directory containing metadata
  if(meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = PI, analyst = analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }

  # add to current gitignore if exists
  if(file.exists(file.path(path, '.gitignore'))){
    gitignore <- readLines(con = file.path(path, '.gitignore'))
  } else {
    gitignore <- NULL
  }

  # add R template gitignore
  # (source: https://github.com/github/gitignore/blob/master/R.gitignore)
  gitignore <- paste0(c(gitignore,
                        "# History files",
                        ".Rhistory",
                        ".Rapp.history",

                        "# Session Data files",
                        ".RData",

                        "# User-specific files",
                        ".Ruserdata",

                        "# Example code in package build process",
                        "*-Ex.R",

                        "# Output files from R CMD build",
                        "/*.tar.gz",

                        "# Output files from R CMD check",
                        "/*.Rcheck/",

                        "# RStudio files",
                        ".Rproj.user/",

                        "# produced vignettes",
                        "vignettes/*.html",
                        "vignettes/*.pdf",

                        paste0("# OAuth2 token, see https://github.com/",
                               "hadley/httr/releases/tag/v0.3"),
                        ".httr-oauth",

                        "# knitr and R markdown default cache directories",
                        "/*_cache/",
                        "/cache/",

                        "# Temporary files created by R markdown",
                        "*.utf8.md",
                        "*.knit.md"), collapse = '\n')

  # by file type
  gitignore <- paste0(c(gitignore,
                          "# R Data files",
                          "*.RData",
                          "*.rda",
                          "*.rdata",
                          "*.rda",
                          "# Text files",
                          "*.csv",
                          "*.txt",
                          "*.dat",
                          "# Excel",
                          "*.xls*",
                          "# SAS",
                          "*.sas7bdat",
                          "*.xport",
                          "# Access",
                          "*.mdb"), collapse = '\n')

  # by Folder
   gitignore <- paste0(c(gitignore,
                          "DataRaw/*",
                          "DataProcessed/*",
                          "!*/ReadMe.md"), collapse = '\n')

   writeLines(gitignore, con = file.path(path, '.gitignore'))

   ## Copy over SOW
   message("Project created. Please remember to copy the scope of work to to Admin/ subdirectory.")

   invisible(template)
}
