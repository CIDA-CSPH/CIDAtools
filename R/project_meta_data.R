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
  analyst_name <- check_string_param_value(analyst_name,'analyst_name')
  set_project_data('analyst', analyst_name)
  return(paste('The Project Analyst Name has been changed to', analyst_name))
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
  project_name <- check_string_param_value(project_name,'project_name')
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
  pi <- check_string_param_value(pi,'PI')
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
  path <- check_string_param_value(path,'path')
  path <- proj_location_handler(path)
  set_project_data('datalocation', path)
  return(paste('The Project Location has been changed to', path))
}

#' Set Project GitHub Location
#'
#' This function allows you to set the Project's GitHub location.
#' This will overwrite the current value if exists.
#'
#' @param git_url A string containing the URL to the GitHub repository for this project.
#' @return A message stating the name has been changed.
#' @keywords options location ProjData
#' @export
#'
set_project_github <- function(git_url=''){
  git_url <- check_string_param_value(git_url,'git_url')
  set_project_data('git_url', git_url)
  return(paste('The Project Location has been changed to', path))
}

#' Get Project GitHub Location
#'
#' This function returns the Project GitHub location or blank if it's not set.
#'
#' @return A character string with the project GitHub
#' @keywords options ProjData ProjectGitHub
#' @export
#'

get_project_github <- function(){
  git_url <- get_project_data('git_url')
  return(git_url)
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


#' Sets the default full path to the project.
#'
#' @param path full path to the project folder
#'
#' @return message indicating the path has been saved.
#' @export
#'
#'
set_full_project_path <- function(path=''){
  path <- check_string_param_value(path,'default_full_path_to_project')
  set_project_data('default_full_path_to_project', path)
  return(paste('The project default full path has been changed to', path))
}

#' Gets the currently set full path to the project
#'
#' @return full path to project
#' @export
#'
get_full_project_path <- function(){
  project_path <- get_project_data('default_full_path_to_project')
  return(project_path)
}


#' Set data for project
#'
#' Allows you to set misc project data parameters
#' for Project Name, Analyst, or PI recommend you use specific function
#'
#'
#' @param parameter Project Parameter to be set
#' @param value Value to set to project parameter
#' @export
#'
#'

set_project_data <- function(parameter, value){
  parameter <- check_string_param_value(parameter,'parameter')
  value <- check_string_param_value(value,'value')
  if(parameter=='datalocation'){
    value <- proj_location_handler(value)
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
#' Allows you to get any project data parameters or all parameters.  Either specify
#' the desired parameter or with no parameter it will return all available parameters.
#'
#' Possible parameters values include:
#'
#'  - analyst - Analyst's Name
#'
#'  - ProjectName - Project Name
#'
#'  - PI - PI Name
#'
#'  - datalocation - Poject folder location under the CIDA PATH.
#'
#'  - default_full_path_to_project - Project default path which is the default full path to the project files.  Includes the local filesystem path to network mount point and network path to project folder.
#'
#'  - git_url - GitHub URL for the project code
#'
#'
#' @param param Project parameter to return or if not specified to return all parameter/value pairs.
#' @export
#'
get_project_data <- function(param=''){
  value <- ''
  project_data <- get_full_project_data()
  if(is.null(param) || param==''){

    value <- project_data

  }else{

    if( !is.null(project_data)){
      value <- project_data
      if(param %in% names(project_data)){
        value <- project_data[[param]]
      }else{
        warning(paste(c(param," not found in project data.")),call.=FALSE,immediate. = TRUE)
      }
    }else{
      warning(paste(c("get_project_data(",param,") returned NULL project data.")),call.=FALSE,immediate. = TRUE)
    }

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



