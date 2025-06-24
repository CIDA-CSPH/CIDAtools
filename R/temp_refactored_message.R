# Functions to warn users of refactored methods and provide the renamed function.
# These are expected to be short term/temporary functions to aid in transition
# after refactoring to provide consistent naming across the package.
# Perhaps remove following deprecation of the methods marked for deprecation.


#'
#' @noMd
#' @noRd

CIDA_drive_path <- function() {
  warning( paste(c("CIDA_drive_path() renamed to get_project_drive_path()")),immediate. = TRUE)
}


#'
#' @noMd
#' @noRd

SetProjectName <- function(){
  warning( paste(c("SetProjectName() renamed to set_project_name()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
SetProjectAnalyst <- function(){
  warning( paste(c("SetProjectAnalyst() renamed to set_project_analyst()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
SetProjectName <- function(){
  warning( paste(c("SetProjectName() renamed to set_project_name()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
SetProjectPI <- function(PI){
  warning( paste(c("SetProjectPI() renamed to set_project_pi()")),immediate. = TRUE)
}


#'
#' @noMd
#' @noRd
SetProjectLocation <- function(){
  warning( paste(c("SetProjectLocation() renamed to set_project_location()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
ProjectAnalyst <- function(){
  warning( paste(c("ProjectAnalyst() renamed to get_project_analyst()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
ProjectName <- function(){
  warning( paste(c("ProjectName() renamed to get_project_name()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
ProjectPI <- function(){
  warning( paste(c("ProjectPI() renamed to get_project_pi()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
ProjectLocation <- function(){
  warning( paste(c("ProjectLocation() renamed to get_project_location()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
SetProjectData <- function(){
  warning( paste(c("SetProjectData() renamed to set_project_data()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd

getProjectData <- function(param){
  warning( paste(c("getProjectData() renamed to get_project_data()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
setAnalyst <- function(){
  warning( paste(c("setAnalyst() renamed to set_project_analyst() or set_default_analyst()")),immediate. = TRUE)
}

#'
#' @noMd
#' @noRd
setPermanentAnalyst <- function(){
  warning( paste(c("setPermanentAnalyst() renamed to set_project_analyst() or set_global_default_analyst()")),immediate. = TRUE)
}


#'
#' @noMd
#' @noRd
removeAnalyst <- function(){
  warning( paste(c("removeAnalyst() renamed to set_project_analyst() or remove_global_default_analyst()")),immediate. = TRUE)
}
