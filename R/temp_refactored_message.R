# Functions to warn users of refactored methods and provide the renamed function.
# These are expected to be short term/temporary functions to aid in transition
# after refactoring to provide consistent naming across the package.
# Perhaps remove following deprecation of the methods marked for deprecation.


#' @inherit get_project_drive_path
#' @export
CIDA_drive_path <- function(file="") {
  renamed_warn(function_name="CIDA_drive_path", replacement_name="get_project_drive_path")
  #warning( paste(c("CIDA_drive_path() renamed to get_project_drive_path()")),immediate. = TRUE)
  return(get_project_drive_path(file))
}


#' @inherit set_project_name
#' @export
SetProjectName <- function(project_name){
  renamed_warn(function_name="SetProjectName", replacement_name="set_project_name")
  #warning( paste(c("SetProjectName(ProjectName) renamed to set_project_name(project_name)")),immediate. = TRUE)
  return(set_project_name(project_name))
}

#' @inherit set_project_analyst
#' @export
SetProjectAnalyst <- function(analyst_name){
  renamed_warn(function_name="SetProjectAnalyst", replacement_name="set_project_analyst")
  #warning( paste(c("SetProjectAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(analyst_name))
}


#' @inherit set_project_pi
#' @export
SetProjectPI <- function(pi){
  renamed_warn(function_name="SetProjectPI", replacement_name="set_project_pi")
  #warning( paste(c("SetProjectPI() renamed to set_project_pi()")),immediate. = TRUE)
  return(set_project_pi(pi))
}


#' @inherit set_project_location
#' @export
SetProjectLocation <- function(path){
  renamed_warn(function_name="SetProjectLocation", replacement_name="set_project_location")
  #warning( paste(c("SetProjectLocation() renamed to set_project_location()")),immediate. = TRUE)
  return(set_project_location(path))
}

#' @inherit get_project_analyst
#' @export
ProjectAnalyst <- function(){
  renamed_warn(function_name="ProjectAnalyst", replacement_name="get_project_analyst")
  #warning( paste(c("ProjectAnalyst() renamed to get_project_analyst()")),immediate. = TRUE)
  return(get_project_analyst())
}

#' @inherit get_project_name
#' @export
ProjectName <- function(){
  renamed_warn(function_name="ProjectName", replacement_name="get_project_name")
  #warning( paste(c("ProjectName() renamed to get_project_name()")),immediate. = TRUE)
  return(get_project_name())
}

#' @inherit get_project_pi
#' @export
ProjectPI <- function(){
  renamed_warn(function_name="ProjectPI", replacement_name="get_project_pi")
  #warning( paste(c("ProjectPI() renamed to get_project_pi()")),immediate. = TRUE)
  return(get_project_pi())
}

#' @inherit get_project_location
#' @export
ProjectLocation <- function(){
  renamed_warn(function_name="ProjectLocation", replacement_name="get_project_location")
  #warning( paste(c("ProjectLocation() renamed to get_project_location()")),immediate. = TRUE)
  return(get_project_location())
}

#' @inherit set_project_meta_data
#' @export
SetProjectData <- function(parameter,value){
  renamed_warn(function_name="SetProjectData", replacement_name="set_project_meta_data")
  #warning( paste(c("SetProjectData() renamed to set_project_meta_data()")),immediate. = TRUE)
  set_project_meta_data(parameter,value)
}

#' @inherit get_project_meta_data
#' @export
getProjectData <- function(param){
  renamed_warn(function_name="getProjectData", replacement_name="get_project_meta_data")
  #warning( paste(c("getProjectData() renamed to get_project_meta_data()")),immediate. = TRUE)
  return(get_project_meta_data(param))
}

#' @inherit set_project_analyst
#' @export
setAnalyst <- function(analyst_name){
  renamed_warn(function_name="setAnalyst", replacement_name="set_project_analyst")
  #warning( paste(c("setAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(analyst_name))
}

#' @inherit set_global_default_analyst
#' @export
setPermanentAnalyst <- function(analyst_name, update_template=FALSE){
  renamed_warn(function_name="setPermanentAnalyst", replacement_name="set_global_default_analyst")
  #warning( paste(c("setPermanentAnalyst() renamed to set_global_default_analyst()")),immediate. = TRUE)
  return(set_global_default_analyst(analyst_name, update_template))
}


#' @inherit remove_global_default_analyst
#' @export
removeAnalyst <- function(){
  renamed_warn(function_name="removeAnalyst", replacement_name="remove_global_default_analyst")
  warning( paste(c("removeAnalyst() renamed to remove_global_default_analyst()")),immediate. = TRUE)
  return(remove_global_default_analyst())
}
