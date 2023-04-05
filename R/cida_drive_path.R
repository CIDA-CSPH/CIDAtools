#' Get CIDA drive path
#'
#' This function attempts to get the proper path for the CIDA drive either on
#' Windows or Mac.
#'
#' @param file (optional) Path to subdirectory/file within CIDA drive
#'
#' @return Full (absolute) file path of CIDA drive
#' @export
#'
#' @examples
#' # Read data from P1234PIname project
#' \dontrun{
#' df <- read.csv(CIDA_drive_path("BRANCHES/Pulmonary/P1234PIname/DataRaw/data.csv"))
#' }
#'

CIDA_drive_path <- function(file = "") {

  # Get operating system (note that MacOS and Linux return unix)
  os <- .Platform$OS.type

  if (os == "unix") { # MacOS/Linux

    # Two potential places drive could exist
    if (dir.exists("/Volumes/sph-cida")) {
      path <- "/Volumes/sph-cida"
    } else {
      stop("Nothing found at /Volumes/sph-cida.",
           " Please ensure drive is mounted and you have entered your",
           " password to access the drive (and are logged into the VPN if",
           " needed.)")
    }

  } else if (os == "windows") { # Windows

    # Only one spot drive can be mounted for Windows
    if (dir.exists("P:/")) {
      path <- "P:/"
    } else {
      stop("Nothing found at P:/.",
           " Please ensure drive is mounted and you have entered your",
           " password to access the drive (and are logged into the VPN if",
           " needed.)")
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
