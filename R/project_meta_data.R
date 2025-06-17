#' Set Project Analyst
#'
#' This function allows you to set the  project analyst.
#' This will overwrite the current value if exists.
#'
#' @param analyst_name A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst ProjData
#' @export
#'
set_project_analyst <- function(analyst_name){
  if(!is.character(analyst_name)) stop('Analyst Name must be a character string')
  if(length(analyst_name) > 1) {
    warning('Only First String is Used')
    analyst_name <- analyst_name[1]
  }
  set_project_data('analyst', analyst_name)
  return(paste('The Project Analyst name has been changed to', analyst_name))
}

#' Set Project Name
#'
#' This function allows you to set the  project name. This will overwrite the
#' current value if exists.
#'
#' @param project_name A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options ProjectName ProjData
#' @export
#'
set_project_name <- function(project_name){
  if(!is.character(project_name)) stop('Project Name must be a character string')
  if(length(project_name) > 1) {
    warning('Only First String is Used')
    project_name <- project_name[1]
  }
  set_project_data('ProjectName', project_name)
  return(paste('The project name has been changed to', project_name))
}

#' Set PI Name
#'
#' This function allows you to set the Project's PI. This will overwrite the
#' current value if exists.
#'
#' @param pi A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options PI ProjData
#' @export
#'
set_project_pi <- function(pi){
  if(!is.character(pi)) stop('PI Name must be a character string')
  if(length(pi) > 1) {
    warning('Only First String is Used')
    pi <- pi[1]
  }
  set_project_data('PI', pi)
  return(paste('The Project PI has been changed to', pi))
}

#' Set Project Location
#'
#' This function allows you to set the Project's location on the CIDA drive.
#' This will overwrite the current value if exists.
#'
#' @param path A string containing the file path to the project location on CIDA drive
#' @return A message stating the name has been changed.
#' @keywords options location ProjData
#' @export
#'
set_project_location <- function(path){
  if(!is.character(path)) stop('Path must be a character string')
  if(length(path) > 1) {
    warning('Only First String is Used')
    path <- path[1]
  }
  path <- proj.location.handler(path)
  set_project_data('datalocation', path)
  return(paste('The Project Location has been changed to', path))
}

#' Get Project Analyst
#'
#' This function returns the Project Analyst Name. If none exists, it
#' will return the value of CIDAtools.analyst option or blank if the option
#' is not set.
#'
#' @return A character string with the analyst name
#' @keywords options Analyst ProjData
#' @export
#'

get_project_analyst <- function(){
  analyst <- get_project_data('analyst')
  if(analyst==""){
    if(!is.null(getOption('CIDAtools.analyst'))){
      analyst <- getOption('CIDAtools.analyst')
    }
  }
  return(analyst)
}

#' Get Project Name
#'
#' This function returns the Project Name or blank if none exists.
#'
#' @return A character string with the project name
#' @keywords options ProjData ProjectName
#' @export
#'

get_project_name <- function(){
  project_name <- get_project_data('ProjectName')
  return(project_name)
}

#' Get PI Name
#'
#' This function returns the PI Name or blank if none exists.
#'
#' @return A character string with the PI name
#' @keywords options ProjData PI
#' @export
#'

get_project_pi <- function(){
  project_pi <- get_project_data('PI')
  return(project_pi)
}

#' Get Project data location on CIDA Drive
#'
#' @param path (optional) a relative path to a particular place in the project
#'
#' @return full (absolute) file path including the project location on CIDA drive
#' @export
#'
#' @examples
#' # Read data from current project
#' \dontrun{
#' df <- read.csv(ProjectLocation("DataRaw/my_proj_data.csv"))
#' }
#'

get_project_location <- function(path = ''){
  temp_path <- get_project_data('datalocation')
  full_path <- ""
  if( temp_path!="" ){
    full_path <- file.path(temp_path, path)
  }else{
    message('Project location not found, use set_project_data("datalocation", x).')
  }
  return(full_path)
}

#' Set data for project
#'
#' Allows you to set misc project data parameters
#' for Project Name, Analyst, or PI recommend you use specific function
#'
#' @param parameter Project Parameter to be set
#' @param value Value to set to project parameter
#' @export
#'
#'

set_project_data <- function(parameter, value){
  if (!is.character(parameter) | !is.character(parameter))
    stop('Parameter must be a character string of length one')
  if(!is.character(value)) stop('Value must be a character string')
  if(length(value) > 1) {
    warning('Only First String is Used')
    value <- value[1]
  }
  if(parameter=='datalocation'){
    value <- proj.location.handler(value)
  }

  proj_data <- get_full_project_data()
  ##Create .ProjData/Data.dcf
  ## TODO This assumes location is the top level directory of the project.
  #       Update so this works from anywhere but only saves to the top level
  #       project directory.
  if(is.null(proj_data)){
    dir.create(paste0('.ProjData/'), recursive = T, showWarnings = F)
    proj_data <- list()
    write.dcf(proj_data, file.path('.ProjData/Data.dcf'))
  }
  proj_data[parameter] <- value
  write.dcf(proj_data, file.path(get_project_data_path()))
}

#' Get data for project
#'
#' Allows you to get misc project data parameters
#'
#' @param param Project parameter to be gotten
#' @export
#'
get_project_data <- function(param){
  value <- ''
  project_data <- get_full_project_data()
  if( !is.null(project_data)){
    if(param %in% names(project_data)){
      value <- project_data[[param]]
    }else{
      warning(paste(c(param," not found in project data.")),call.=FALSE,immediate. = TRUE)
    }
  }else{
    warning(paste(c("get_project_data(",param,") returned NULL project data.")),call.=FALSE,immediate. = TRUE)
  }
  return(value)
}


#' Internal Function to return Project Data object for use in the other methods
#' that read .ProjData/Data.dcf
#'
#' @noMd
#' @noRd
#'

get_full_project_data <- function(){
  proj_data <- NULL
  path <- get_project_data_path()
  if(is.null(path) || path == "" ){
    warning(".ProjData/Data.dcf file not found in project.",call.=FALSE,immediate. = TRUE)
  }else if(path !=""){
    proj_data <- read.dcf(file.path(path), all = T)
  }
  return(proj_data)
}

#' Internal Function to return Project Data path for use in the other methods
#' that read .ProjData/Data.dcf
#'
#' @noMd
#' @noRd
#'
get_project_data_path <- function(){
  path <- ""

  ## TODO There should be a way to find the top project directory and not use
  #       the ../ relative navigation below that will fail after 3 subfolders.
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    path <- '.ProjData/Data.dcf'
  }else if(file.exists(file.path('../.ProjData/Data.dcf'))){
    path <- '../.ProjData/Data.dcf'
  }else if(file.exists(file.path('../../.ProjData/Data.dcf'))){
    path <- '../../.ProjData/Data.dcf'
  }else if(file.exists(file.path('../../../.ProjData/Data.dcf'))){
    path <- '../../../.ProjData/Data.dcf'
  }else{
    warning(".ProjData/Data.dcf file not found in project.",call.=FALSE,immediate. = TRUE)
  }
  return(path)
}



