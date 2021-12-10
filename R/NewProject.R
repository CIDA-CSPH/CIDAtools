proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)

  ProjectName <- paste0(basename(getwd()), " ", path)

  ### Setup ReadMe Files ----
  readme <- c(paste0("# ", ProjectName, "  "),
              paste0("**PI**: ", dots$PI, "  "),
              paste0("**Analyst**: ", dots$analyst, "  "),
              "",
              "Details about the folders: ",
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
  writeLines(paste0(readme, collapse = '\n'),
             con = file.path(path, "ReadMe.md"))

  # readme files
  CreateReadMe(path = path)

  ### Create Meta File ----
  # for project info
  if(dots$meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = dots$PI, analyst = dots$analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
  ### Setup Git ----

  # add to current gitignore if exists
  if(file.exists(file.path(path, '.gitignore'))){
    gitignore <- readLines(con = file.path(path, '.gitignore'))
  } else {
    gitignore <- NULL
  }
  # add R template gitignore
  # (source: https://github.com/github/gitignore/blob/master/R.gitignore)
  gitignore <- paste0(c(gitignore,
                        "# Random files",
                        "*.DS_Store",
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
  if(dots$nodata == 'By File Type'){
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
    }

    # by Folder
    if(dots$nodata == "By Location"){
      gitignore <- paste0(c(gitignore,
                            "DataRaw/*",
                            "DataProcessed/*",
                            "!*/ReadMe.md"), collapse = '\n')

    }


    # git initialize
    if(dots$git_init){
      if (!requireNamespace('git2r', quietly = T)) {
        warning('git2r is required for git initialization')
      } else{
        tryCatch({
          writeLines(gitignore, con = file.path(path, '.gitignore'))
          repo <- git2r::init(path)
          if(dots$remote_origin != '') git2r::remote_add(repo, 'origin', dots$remote_origin)
          if(dots$initcommit) {
            git2r::add(repo, 'ReadMe.md')
            git2r::commit(repo, message = 'Initial Commit')
            if(dots$remote_origin != '') {
            system(paste('cd', path, '&& git push -u origin master'))
            }
          }
        }, error = function(e){
          paste0('There was an error setting up the git repo',
                 e)
        })
      }
    }
}

