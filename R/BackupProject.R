#'Backup Project Directory
#'
#'This function backs up a CIDA project to the shared (P) CIDA drive. The backup directory
#'can either be existing (in which only changed files/folders are updated), or
#'nonexisting, in which case a full project backup is created.
#'
#'@param subdir_to Subdirectory within shared drive to back up to
#'@param path_from Path from where the folders should be copied (project
#'  directory location).
#'@param path_to Path to where the folders should be copied (P drive, only used
#'  if specified).
#'@param exclude files/folders NOT to be backed up to the P-drive (useful for
#'larger files that don't change often). Currently not used.
#'@param recreate should backup be created from the ground up?
#' (can take longer, but useful for projects with many changes)
#'
#'@return This function has verbose output to ensure the back up is working, and
#'  ultimately returns a success indicator that's returned by file.copy.
#'
#'SP COMMENT 2021-12-09: remove export
#'export
BackupProject <- function(subdir_to = "Projects",
                          path_from = getwd(),
                          path_to = NULL,
                          exclude = c(".DS_Store", ".Rproj.user", ".git"),
                          recreate = FALSE) {

  # Check args, make into absolute paths
  path_from <- normalizePath(path_from)

  # Get proper path to Shared drive
  if(is.null(path_to)) {
    path_to <- file.path("/Volumes/CIDA/", subdir_to)
    if(tolower(Sys.info()['sysname']) == "windows")
      path_to <- file.path("P:/CIDA/Shared", subdir_to)
  }

  # Make sure P drive is mounted
  if(!dir.exists(path_to))
    stop("'", path_to, "' not found. Is the shared drive mounted?",
    "\nIf so, consider specifying with 'path_to' arg.")

  path_to <- normalizePath(path_to)

  ## Check if specific project folder exists on P drive
  backup_path <- file.path(path_to, basename(path_from))
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
