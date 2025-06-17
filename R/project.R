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










