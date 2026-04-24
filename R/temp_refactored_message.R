# Functions to warn users of refactored methods and provide the renamed function.
# These are expected to be short term/temporary functions to aid in transition
# after refactoring to provide consistent naming across the package.
# Perhaps remove following deprecation of the methods marked for deprecation.


#'
#' @noMd
#' @noRd
#' @export
CIDA_drive_path <- function(file="") {
  renamed_warn(function_name="CIDA_drive_path", replacement_name="get_project_drive_path")
  #warning( paste(c("CIDA_drive_path() renamed to get_project_drive_path()")),immediate. = TRUE)
  return(get_project_drive_path(file))
}


#'
#' @noMd
#' @noRd
#' @export
SetProjectName <- function(ProjectName){
  renamed_warn(function_name="SetProjectName", replacement_name="set_project_name")
  #warning( paste(c("SetProjectName(ProjectName) renamed to set_project_name(project_name)")),immediate. = TRUE)
  return(set_project_name(ProjectName))
}

#'
#' @noMd
#' @noRd
#' @export
SetProjectAnalyst <- function(AnalystName){
  renamed_warn(function_name="SetProjectAnalyst", replacement_name="set_project_analyst")
  #warning( paste(c("SetProjectAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(AnalystName))
}


#'
#' @noMd
#' @noRd
#' @export
SetProjectPI <- function(PI){
  renamed_warn(function_name="SetProjectPI", replacement_name="set_project_pi")
  #warning( paste(c("SetProjectPI() renamed to set_project_pi()")),immediate. = TRUE)
  return(set_project_pi(PI))
}


#'
#' @noMd
#' @noRd
#' @export
SetProjectLocation <- function(path){
  renamed_warn(function_name="SetProjectLocation", replacement_name="set_project_location")
  #warning( paste(c("SetProjectLocation() renamed to set_project_location()")),immediate. = TRUE)
  return(set_project_location(path))
}

#'
#' @noMd
#' @noRd
#' @export
ProjectAnalyst <- function(){
  renamed_warn(function_name="ProjectAnalyst", replacement_name="get_project_analyst")
  #warning( paste(c("ProjectAnalyst() renamed to get_project_analyst()")),immediate. = TRUE)
  return(get_project_analyst())
}

#'
#' @noMd
#' @noRd
#' @export
ProjectName <- function(){
  renamed_warn(function_name="ProjectName", replacement_name="get_project_name")
  #warning( paste(c("ProjectName() renamed to get_project_name()")),immediate. = TRUE)
  return(get_project_name())
}

#'
#' @noMd
#' @noRd
#' @export
ProjectPI <- function(){
  renamed_warn(function_name="ProjectPI", replacement_name="get_project_pi")
  #warning( paste(c("ProjectPI() renamed to get_project_pi()")),immediate. = TRUE)
  return(get_project_pi())
}

#'
#' @noMd
#' @noRd
#' @export
ProjectLocation <- function(){
  renamed_warn(function_name="ProjectLocation", replacement_name="get_project_location")
  #warning( paste(c("ProjectLocation() renamed to get_project_location()")),immediate. = TRUE)
  return(get_project_location())
}

#'
#' @noMd
#' @noRd
#' @export
SetProjectData <- function(Parameter,Value){
  renamed_warn(function_name="SetProjectData", replacement_name="set_project_meta_data")
  #warning( paste(c("SetProjectData() renamed to set_project_meta_data()")),immediate. = TRUE)
  set_project_meta_data(Parameter,Value)
}

#'
#' @noMd
#' @noRd
#' @export
getProjectData <- function(param){
  renamed_warn(function_name="getProjectData", replacement_name="get_project_meta_data")
  #warning( paste(c("getProjectData() renamed to get_project_meta_data()")),immediate. = TRUE)
  return(get_project_meta_data(param))
}

#'
#' @noMd
#' @noRd
#' @export
setAnalyst <- function(AnalystName){
  renamed_warn(function_name="setAnalyst", replacement_name="set_project_analyst")
  #warning( paste(c("setAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(AnalystName))
}

#'
#' @noMd
#' @noRd
#' @export
setPermanentAnalyst <- function(AnalystName){
  renamed_warn(function_name="setPermanentAnalyst", replacement_name="set_global_default_analyst")
  #warning( paste(c("setPermanentAnalyst() renamed to set_global_default_analyst()")),immediate. = TRUE)
  return(set_global_default_analyst(AnalystName))
}


#'
#' @noMd
#' @noRd
#' @export
removeAnalyst <- function(){
  renamed_warn(function_name="removeAnalyst", replacement_name="remove_global_default_analyst")
  warning( paste(c("removeAnalyst() renamed to remove_global_default_analyst()")),immediate. = TRUE)
  return(remove_global_default_analyst())
}
