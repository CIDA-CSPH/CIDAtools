# Functions to warn users of refactored methods and provide the renamed function.
# These are expected to be short term/temporary functions to aid in transition
# after refactoring to provide consistent naming across the package.
# Perhaps remove following deprecation of the methods marked for deprecation.


#'
#' @noMd
#' @noRd

CIDA_drive_path <- function(file="") {
  warning( paste(c("CIDA_drive_path() renamed to get_project_drive_path()")),immediate. = TRUE)
  return(get_project_drive_path(file))
}


#'
#' @noMd
#' @noRd

SetProjectName <- function(ProjectName){
  warning( paste(c("SetProjectName(ProjectName) renamed to set_project_name(project_name)")),immediate. = TRUE)
  return(set_project_name(ProjectName))
}

#'
#' @noMd
#' @noRd
SetProjectAnalyst <- function(AnalystName){
  warning( paste(c("SetProjectAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(AnalystName))
}


#'
#' @noMd
#' @noRd
SetProjectPI <- function(PI){
  warning( paste(c("SetProjectPI() renamed to set_project_pi()")),immediate. = TRUE)
  return(set_project_pi(PI))
}


#'
#' @noMd
#' @noRd
SetProjectLocation <- function(path){
  warning( paste(c("SetProjectLocation() renamed to set_project_location()")),immediate. = TRUE)
  return(set_project_location(path))
}

#'
#' @noMd
#' @noRd
ProjectAnalyst <- function(){
  warning( paste(c("ProjectAnalyst() renamed to get_project_analyst()")),immediate. = TRUE)
  return(get_project_analyst())
}

#'
#' @noMd
#' @noRd
ProjectName <- function(){
  warning( paste(c("ProjectName() renamed to get_project_name()")),immediate. = TRUE)
  return(get_project_name())
}

#'
#' @noMd
#' @noRd
ProjectPI <- function(){
  warning( paste(c("ProjectPI() renamed to get_project_pi()")),immediate. = TRUE)
  return(get_project_pi())
}

#'
#' @noMd
#' @noRd
ProjectLocation <- function(){
  warning( paste(c("ProjectLocation() renamed to get_project_location()")),immediate. = TRUE)
  return(get_project_location())
}

#'
#' @noMd
#' @noRd
SetProjectData <- function(Parameter,Value){
  warning( paste(c("SetProjectData() renamed to set_project_meta_data()")),immediate. = TRUE)
  set_project_meta_data(Parameter,Value)
}

#'
#' @noMd
#' @noRd

getProjectData <- function(param){
  warning( paste(c("getProjectData() renamed to get_project_meta_data()")),immediate. = TRUE)
  return(get_project_meta_data(param))
}

#'
#' @noMd
#' @noRd
setAnalyst <- function(AnalystName){
  warning( paste(c("setAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
  return(set_project_analyst(AnalystName))
}

#'
#' @noMd
#' @noRd
setPermanentAnalyst <- function(AnalystName){
  warning( paste(c("setPermanentAnalyst() renamed to set_global_default_analyst()")),immediate. = TRUE)
  return(set_global_default_analyst(AnalystName))
}


#'
#' @noMd
#' @noRd
removeAnalyst <- function(){
  warning( paste(c("removeAnalyst() renamed to remove_global_default_analyst()")),immediate. = TRUE)
  return(remove_global_default_analyst())
}
