#'
#' get_default_path() - checks the project metadata file then checks user path
#' file to return a default path if no path was found automatically.
#'
#' @return path of project(CIDA) drive
#' @noRd
#' @noMd
#'
#'

get_default_path <- function(){
  path <- ""
  ## TODO. Check project and then check User/Global Default

  # Attempt to load project meta data and pull the path from it.
  project_data=get_full_project_data()
  if(! is.null(project_data) && 'datalocation' %in% names(project_data)){
    path <- project_data['datalocation']
  }
  if(is.null(path) || path==""){
    #Attempt to load the global default path
    path <- get_global_default_path()
  }

  return(path)
}



