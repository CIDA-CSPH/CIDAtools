#'Create Project Directory + readme files
#'
#'This function creates the standard project organization structure for CIDA
#'within a folder that already exists.
#'
#'@param path Where should they be created? Default is the working directory.
#'@param template Which subdirectories to create
#'@param ProjectName Name of project, or "" for blank
#'@param PI Name of PI and credentials, or "" for blank
#'@param analyst Name of Analyst(s), or "" for blank
#'@param datalocation Location of project on CIDA Drive, or "" for blank
#'
#'@return This function creates the desired project subdirectories and readmes,
#'  as well as a standard .gitignore file files. It will not overwrite the file
#'  however if it does not exist. It does not return anything.
#'@keywords project createproject
#'
#'@seealso proj_setup() is the internal wrapper for this that gets called when
#'  using the RStudio GUI to create a project
#'
#'@export
CreateProject <- function(path = getwd(),
  template = c('Admin', 'Background', 'Code', 'DataRaw',
               'DataProcessed', 'Dissemination', 'Reports'),
  ProjectName = "", PI = "", analyst = "", datalocation = "") {

  if(!dir.exists(path))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # has meta been provided?
  meta <- !all(c(ProjectName, PI, analyst, datalocation) %in% "")

  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # Overall readme

  readme <- c(paste0("**Project Name**: ", ProjectName, "  "),
              paste0("**PI**: ", PI, "  "),
              paste0("**Analyst**: ", analyst, "  "),
              paste0("**CIDA drive Location**: ", proj.location.handler(datalocation), "  "),
              paste0("**GitHub Location**: [fill in after setting up remote repository]"),
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
  if(!file.exists(file.path(path, "ReadMe.md")))
    writeLines(paste0(readme, collapse = '\n'),
               con = file.path(path, "ReadMe.md"))

  # Create subdirectory readmes
  create_readme(template = template, path = path)

  # Add .ProjData directory containing metadata
  if(meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = PI,
                     analyst = analyst, datalocation = datalocation)
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
   message("Project created. Please remember to copy the scope of work to to Admin/ subdirectory.")

   invisible(template)
}

proj_setup <- function(path, ...){
  # ensure path exists
  dots <- list(...)
  ProjectName <- paste0(path)

  CreateProject(path, ProjectName = paste0(path), PI = dots$PI,
                analyst = dots$analyst, datalocation = dots$datalocation)

  # for project info
  dir.create(paste0(path, '/.ProjData'))
  ProjData <- list(ProjectName = ProjectName, PI = dots$PI, analyst = dots$analyst, datalocation = dots$datalocation)
  write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))

}

create_readme <- function(template = c('Admin', 'Background', 'Code', 'DataRaw',
                                      'DataProcessed', 'Dissemination',
                                      'Reports'), path = getwd()){
  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # create list with lines for each template
  readme <- list()

  readme$Admin <- c("# Admin  ",
                    "  ",
                    "This folder contains the scope of work and other relevant files from CIDA admin.  ",
                    "  ",
                    "Details about the files:  ",
                    "  ",
                    "File | Description",
                    "---|---------------------------------------------------------------------",
                    "  ",
                    "")
  readme$Background <- c("# Background  ",
                         "  ",
                         "This folder contains documents provided by investigators and the data analysis plan.  ",
                         "  ",
                         "Details about the files:  ",
                         "  ",
                         "File | Description",
                         "---|---------------------------------------------------------------------",
                         "  ")
  readme$Code <- c("This folder contains all the code.  ",
                   "  ",
                   "Details about the files in this folder:",
                   "  ",
                   "File | Description",
                   "---|---------------------------------------------------------------------",
                   "  ")
  readme$DataProcessed <- c("# Processed Data  ",
                            "  ",
                            "Scripts that created the files in this folder:  ",
                            "  ",
                            "File | Script | Description",
                            "---|------------------|---------------------------------------------------",
                            "  ")
  readme$DataRaw <- c("# Raw Data",
                      "  ",
                      "Details about the files:  ",
                      "  ",
                      "File | Details",
                      "---|---------------------------------------------------------------------",
                      "    ",
                      "  ")

  readme$Dissemination <- c("# Dissemination",
                            "  ",
                            "This folder contains abstracts, posters, papers and anything else produced for dissemination.  ",
                            "  ",
                            "Details about the files:  ",
                            "  ",
                            "File | Description",
                            "---|---------------------------------------------------------------------",
                            "  ",
                            "  ")
  readme$Reports <- c("# Reports",
                      "  ",
                      "This folder contains the rmardown scripts and pdf output of reports.  ",
                      "  ",
                      "Details about the files:  ",
                      "  ",
                      "File | Description",
                      "---|---------------------------------------------------------------------",
                      "  ")

  # Function for creating the directory
  createDir <- function(x){
    paste0(path, '/', x)
  }

  createFiles <- function(x){
    file.path(path, paste0(x, '/ReadMe.md'))
  }

  readme <- readme[template]

  pathnames <- sapply(names(readme), createDir)
  dir_created <- lapply(pathnames, dir.create, showWarnings = F, recursive = T)
  con <- lapply(names(readme), createFiles)
  doNotOverwrite <- sapply(con, file.exists)
  readme <- readme[!doNotOverwrite]
  con <- con[!doNotOverwrite]
  files_created <- mapply(writeLines, lapply(readme, paste0, collapse = '\n'), con)
}
