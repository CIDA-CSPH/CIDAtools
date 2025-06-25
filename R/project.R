#' Get Project drive path
#'
#' This function attempts to get the proper path for the Project(CIDA) drive either on
#' Windows or Mac automatically.  If the expected path is not found it tried to
#' load the project metadata path and if that fails it looks for a global default path
#' in the user cida_defaults.dcf file.
#'
#' @param file (optional) Path to subdirectory/file within the main project(CIDA) drive
#'
#' @return Full (absolute) file path of project(CIDA) drive
#' @export
#'
#' @examples
#' # Read data from P1234PIname project
#' \dontrun{
#' df <- read.csv(get_project_drive_path("BRANCHES/Pulmonary/P1234PIname/DataRaw/data.csv"))
#' }
#'

get_project_drive_path <- function(file = "") {
  path <- ""

  # Get operating system (note that MacOS and Linux return unix)
  os <- .Platform$OS.type


  ## TODO Set a global default path somewhere and then iterativly parse each
  # sub-directory to test instead of these static sub-directories of the CIDA path

  if (os == "unix") { # MacOS/Linux

    # Four potential places drive could exist based on path used for mapping
    # and case sensitivity of the file system
    # Then check manually set project data in .ProjData/Data.dcf
    if (dir.exists("/Volumes/sph-cida")) {
      path <- "/Volumes/sph-cida/CIDA"
    } else if(dir.exists("/Volumes/cida")){
      path <- "/Volumes/cida"
    }else if(dir.exists("/Volumes/sph")){
      path <- "/Volumes/sph/SPH-CIDA/CIDA"
    }else if(dir.exists("/Volumes/dept")){
      path <- "/Volumes/dept/SPH/SPH-CIDA/CIDA"
    }else if (dir.exists("/Volumes/SPH-CIDA")) {
      path <- "/Volumes/SPH-CIDA/CIDA"
    } else if(dir.exists("/Volumes/CIDA")){
      path <- "/Volumes/CIDA"
    }else if(dir.exists("/Volumes/SPH")){
      path <- "/Volumes/SPH/SPH-CIDA/CIDA"
    }else if(dir.exists("/Volumes/DEPT")){
      path <- "/Volumes/DEPT/SPH/SPH-CIDA/CIDA"
    }else {
      path <- get_default_path()
      if(path==""){
        stop("Nothing found at /Volumes/dept || SPH || SPH-CIDA || CIDA",
             " Please ensure drive is mounted and you have entered your",
             " password to access the drive (and are logged into the VPN if",
             " needed.)",
             " If still experiencing issues try set_project_data_path() or ",
             " set_global_default_path()"
             )
      }else{
        if(! dir.exists(path)){
          stop("Automatic Path: Failed\nDefault Path:",path,": Failed\n",
               " If still experiencing issues try set_project_data_path() or ",
               " set_global_default_path()")
        }
      }
    }

  } else if (os == "windows") { # Windows

    # Only one spot drive can be mounted for Windows
    if (dir.exists("P:/")) {
      path <- "P:/"
      if(dir.exists("P:/dept/SPH/SPH-CIDA/CIDA")){
        path <- "P:/dept/SPH/SPH-CIDA/CIDA"
      }else if(dir.exists("P:/SPH/SPH-CIDA/CIDA")){
        path <- "P:/SPH/SPH-CIDA/CIDA"
      }else if(dir.exists("P:/SPH-CIDA/CIDA")){
        path <- "P:/SPH-CIDA/CIDA"
      }else if(dir.exists("P:/CIDA")){
        path <- "P:/CIDA"
      }
    }else {
      path <- get_default_path()
      if(path==""){
        stop("Nothing found at P:/.",
             " Please ensure drive is mounted and you have entered your",
             " password to access the drive (and are logged into the VPN if",
             " needed.)")
      }else{
        if(! dir.exists(path)){
          stop("Automatic Path: Failed\nDefault Path:",path,": Failed (does not exist)\n",
               " If still experiencing issues try set_project_data_path() or ",
               " set_global_default_path()")
        }
      }
    }
  } else {

    stop("Operating system could not be identified")

  }

  # Combine CIDA drive path with user provided subdirectory/file
  file_path <- file.path(path, file)

  # Check if full path exists (first as file, second as directory)
  if (!dir.exists(file_path) & !file.exists(file_path)) {

    # TODO: consider adding function to search for partial paths and suggest
    #   alternatives

    stop("Nothing found at path ", file_path,
         "\nCheck spelling of path, and ensure drive is mounted and you have",
         " entered your password to access the drive (and are logged into the",
         " VPN if needed.)")
  }

  # Return full path
  return(file_path)
}

#' Rsync type backup of project updated files?
#' files copied in both directions flag/skip conflicting files?
rsync_project <- function(){

}
#' copy local files to network path
#' prompt for updated remote files
new_backup_project <- function(path_from = getwd(),
                           path_to = NULL,
                           exclude = c(".DS_Store", ".Rproj.user", ".git"),
                           recreate = FALSE,
                           data_only = TRUE,
                           readme = TRUE) {

  # check for full absolute paths at source and destination
  path <- fs::path_real("~/CIDA_test/ProjectSM1/data/All_tissue_perc_trx.svg") # Note path_real requires existence
  # check for changed files
  checksum <- digest::digest(path,algo="sha256",file=TRUE)
  # print/list changes to be made
    #Bidirectional
  #rename local files before overwrite to allow undo
  #copy remote files
  #if successful allow removing undo files
}

#' copy local files to network path
#' prompt for updated remote files
clear_project_undo_cache <- function(){

}

#'Create Project Directory + readme files
#'
#'This function creates the standard project organization structure for CIDA
#'within a folder that already exists.
#'
#'@param path Where should they be created? Default is the working directory.
#'@param template Which subdirectories to create
#'@param project_name Name of project, or "" for blank
#'@param pi Name of PI and credentials, or "" for blank
#'@param analyst Name of Analyst(s), or "" for blank
#'@param data_location Location of project on CIDA Drive, or "" for blank
#'@param git_location Location project on GitHub
#'@return This function creates the desired project subdirectories and readmes,
#'  as well as a standard .gitignore file files. It will not overwrite the file
#'  however if it does not exist. It does not return anything.
#'@keywords project createproject
#'
#'@seealso proj_setup() is the internal wrapper for this that gets called when
#'  using the RStudio GUI to create a project
#'
#'@export
create_project <- function(path = getwd(),
                           template = c('Admin', 'Background', 'Code', 'DataRaw',
                                        'DataProcessed', 'Dissemination', 'Reports'),
                           project_name = "", pi = "", analyst = "", data_location = "",
                           git_location = "") {

  if(!dir.exists(path))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # has meta been provided?
  meta <- !all(c(project_name, pi, analyst, data_location,git_location) %in% "")

  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # Overall readme

  readme <- c(paste0("**Project Name**: ", project_name, "  "),
              paste0("**PI**: ", pi, "  "),
              paste0("**Analyst**: ", analyst, "  "),
              paste0("**CIDA drive Location**: ", proj_location_handler(data_location), "  "),
              paste0("**GitHub Location**: ", git_location, "  "),
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
    proj_data <- list(ProjectName = project_name, PI = pi,
                     analyst = analyst, datalocation = data_location,
                     gitlocation = git_location)
    write.dcf(proj_data, file.path(path, '/.ProjData/Data.dcf'))
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
  project_name <- paste0(path)

  create_project(path, project_name = project_name, pi = dots$PI,
                 analyst = dots$analyst, data_location = dots$datalocation,
                 git_location = dots$gitlocation)

  # for project info
  dir.create(paste0(path, '/.ProjData'))
  proj_data <- list(ProjectName = project_name, PI = dots$PI,
                   analyst = dots$analyst, datalocation = dots$datalocation,
                   gitlocation = dots$gitlocation)
  write.dcf(proj_data, file.path(path, '/.ProjData/Data.dcf'))

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
                      "This folder contains the rmarkdown scripts and pdf output of reports.  ",
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


#' pull project files from remote directory
#'

#'Backup Project Directory
#'
#'This function backs up a CIDA project to the shared (P) CIDA drive. The backup directory
#'can either be existing (in which only changed files/folders are updated), or
#'nonexisting, in which case a full project backup is created.
#'
#'@param path_from Path from where the folders should be copied (project
#'  directory location).
#'@param path_to Path to where the folders should be copied (P drive, only used
#'  if specified).
#'@param exclude files/folders NOT to be backed up to the P-drive (useful for
#'larger files that don't change often). Currently not used.
#'@param recreate should backup be created from the ground up?
#' (can take longer, but useful for projects with many changes)
#' @param data_only should only subdirs including "data" (DataRaw/ and DataProcessed/) be backed up?
#' @param readme forces backup of project readme
#' @return This function has verbose output to ensure the back up is working, and
#'  ultimately returns a success indicator that's returned by file.copy.
#'
#'@export
backup_project <- function(path_from = getwd(),
                           path_to = NULL,
                           exclude = c(".DS_Store", ".Rproj.user", ".git"),
                           recreate = FALSE,
                           data_only = TRUE,
                           readme = TRUE) {

  # Check args, make into absolute paths
  path_from <- normalizePath(path_from)

  # Get proper path to Shared drive
  if(missing(path_to)) {
    path_to <- get_project_location()
    if(path_to == "")
      stop("Please first set project location, e.g., CIDAtools::set_project_location('Branches/EmergencyMedicine/ThisProject')")

    if(!dir.exists(CIDAtools::get_project_drive_path()))
      stop("Please ensure the CIDA drive is mounted, or set `path_to`")

  }

  path_to <- normalizePath(path_to)

  ## Check if specific project folder exists on P drive
  backup_path <- file.path(path_to)
  if(!dir.exists(backup_path)) {
    message("Note: '", backup_path, "' not found; directory was created.")
    dir.create(backup_path)
  } else if (!recreate){
    message("Note: backup path already exists, and will be updated unless cancelled.")
    message("Backup path:\n", backup_path,
            "\nProject path:\n",
            path_from)
  } else {
    message("Note: backup path already exists, and will be completely overwritten since recreate == TRUE.")
    message("Backup path:\n", backup_path,
            "\nProject path:\n",
            path_from, "\n\nType 'yes' to confirm.")
    delete_old <- readline()

    if(delete_old != "yes")
      stop("Cancelled")
    unlink(backup_path, recursive=TRUE)
    dir.create(backup_path)
  }

  message("\nDetermining current backup situation...")

  files_to_copy <-
    list.files(path_from, recursive = T, all.files = T)
  dirs_to_copy <- list.dirs(path_from, recursive = T, full.names = F)[-1]

  if(length(exclude)) {
    files_to_exclude <- c(
      unlist(sapply(exclude[dir.exists(exclude)], list.files, recursive = TRUE,
                    all = TRUE, full.names = T)),
      exclude[!dir.exists(exclude)])

    files_to_copy <- files_to_copy[!(files_to_copy %in% files_to_exclude)]

    dirs_to_exclude <- c(
      unlist(sapply(exclude[dir.exists(exclude)], list.dirs, recursive = TRUE,
                    full.names = T)))

    dirs_to_copy <- dirs_to_copy[!(dirs_to_copy %in% dirs_to_exclude)]
  }

  if(data_only) {
    string_matches <- "dataraw|dataprocessed"
    if(readme)
      string_matches <- "readme|dataraw|dataprocessed"

    # find large files (>= 250 MB)
    large_idx <- file.size(files_to_copy)/1e6 >= 250

    # find file matches
    file_matches <- grepl(string_matches, files_to_copy, ignore.case = TRUE)
    dir_matches <- grepl(string_matches, dirs_to_copy, ignore.case = TRUE)

    files_to_copy <- files_to_copy[large_idx | file_matches]
    dirs_to_copy <- dirs_to_copy[large_idx | dir_matches]
  }

  ## Check if any files can be ignored using time last modified time
  check <- file.exists(file.path(backup_path, files_to_copy))
  if(any(check)) {
    to_mtime <- file.mtime(file.path(backup_path, files_to_copy))

    # If no file found, set last modified time into future (kind of a hack)
    to_mtime[is.na(to_mtime)] <- Sys.time() +500

    from_mtime <- file.mtime(file.path(path_from, files_to_copy))
    files_to_copy <- files_to_copy[abs(difftime(to_mtime, from_mtime, units = "secs")) > 1]
  }

  # Check and don't copy dirs if they already exist
  dirs_to_copy <- dirs_to_copy[!dir.exists(file.path(backup_path, dirs_to_copy))]

  message("\nI'm about to create or update ",length(dirs_to_copy)," subdirectories and ",
          length(files_to_copy), " files.",
          "'\nType 'yes' to confirm, or 'list' to list changes.")
  val <- readline()
  if(val == "list") {
    cat("Subdirs:", dirs_to_copy, sep = "\n")
    cat("\n\nFiles:", files_to_copy, sep = "\n")
    message("'\n\n Type 'yes' to confirm.")
    val <- readline()
  }

  stopifnot(val == "yes")

  if(length(dirs_to_copy)) {
    message("Creating ", length(dirs_to_copy)," subdirectories...")
    pb <- dplyr::progress_estimated(length(dirs_to_copy))
    r1 <- sapply(1:length(dirs_to_copy), function(i) {
      pb$tick()$print()
      dir.create(file.path(backup_path, dirs_to_copy[i]))
    })
  } else
    r1 <- T

  if(length(files_to_copy)) {
    message("\nCopying/updating ", length(files_to_copy), " files...")
    pb <- dplyr::progress_estimated(length(files_to_copy))
    r2 <- sapply(1:length(files_to_copy), function(i) {
      pb$tick()$print()
      file.copy(file.path(path_from, files_to_copy[i]),
                file.path(backup_path, files_to_copy[i]),
                overwrite = TRUE, copy.date = TRUE)

    })
  } else
    r2 <- T

  result <- all(r1) & all(r2)
  create_backup_info(backup_path)

  return(invisible(result))
}

create_backup_info <- function(path) {
  fileConn<- file(file.path(path, "backup_info.md"))
  lines <- c(
    "This is a backup of the actual project directory. ",
    "",
    "DO NOT EDIT THIS DIRECTORY.",
    "",
    "If you do, changes may be overwritten by future backups.",
    "",
    paste0("This directory was last backed up at ", Sys.time()),
    ""
  )
  writeLines(lines, fileConn)
  close(fileConn)
}









