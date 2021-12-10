#'Create Project Directory + readme files
#'
#'This function creates the standard project organization structure for CIDA
#'within a folder that already exists.
#'
#'@param template Which subdirectories to create
#'@param path Where should they be created? Default is the working directory.
#'@param ProjectName Name of project, which should be the year of the dataset; "" for blank
#'@param PI Name and credentials of PI who provided raw data, i.e., lead contact for data questions; "" for blank
#'@param analyst Name of Analyst(s) who wrote the analysis code, i.e., lead contact for code questions; "" for blank
#'
#'@return This function creates the desired project subdirectories and readmes,
#'  as well as a standard .gitignore file files. It will not overwrite the file
#'  however if it does not exist. It does not return anything.
#'@keywords project createproject
#'
#'@export
CreateProject <- function(
  template = c('01_Raw_data',
               '02_Analysis',
               '03_Working_files',
               '04_Processed',
               '05_Admin'),
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
              paste("01_Raw_data | contains all raw data (metadata and",
                    "GPS data) provided by data contact"),
              "02_Analysis | contains all R scripts for this project",
              paste("03_Working_files | contains all data produced by analysis",
                    "scripts, but not final data products; e.g., tidied data"),
              paste("04_Processed | contain all final processed data outputs for",
                    "distribution; e.g. Movebank"),
              paste("05_Admin | contains miscellaneous administrative files,",
                    "e.g. Abstracts, Reports, Papers")
              )


  # write to readme file
  if(!file.exists(file.path(path, "ReadMe.md")))
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
                          "01_Raw_data/*",
                          "03_Working_files/*",
                          "04_Processed/*",
                          "!*/ReadMe.md"), collapse = '\n')

   writeLines(gitignore, con = file.path(path, '.gitignore'))

   # Create .Rproj file
   rproj <- paste0(c("Version: 1.0",
                     "",
                     "RestoreWorkspace: Default",
                     "SaveWorkspace: Default",
                     "AlwaysSaveHistory: Default",
                     "",
                     "EnableCodeIndexing: Yes",
                     "UseSpacesForTab: Yes",
                     "NumSpacesForTab: 2",
                     "Encoding: UTF-8",
                     "",
                     "RnwWeave: knitr",
                     "LaTeX: pdfLaTeX"), collapse = "\n")

   if(!file.exists(file.path(path, paste0(basename(path), ".Rproj"))))
      writeLines(rproj, con = file.path(path, paste0(basename(path), ".Rproj")))

   ## Copy over SOW
   message("Project created. Go process some bird tracks!")

   invisible(template)
}

