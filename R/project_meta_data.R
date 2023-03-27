#' Set Project Analyst
#'
#' This function allows you to set the  project analyst.
#' This will overwrite the current value if exists.
#'
#' @param AnalystName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst ProjData
#' @export
#'
SetProjectAnalyst <- function(AnalystName){
  if(!is.character(AnalystName)) stop('Analyst Name must be a character string')
  if(length(AnalystName) > 1) {
    warning('Only First String is Used')
    AnalystName <- AnalystName[1]
  }
  SetProjectData('analyst', AnalystName)
  return(paste('The Project Analyst name has been changed to', AnalystName))
}

#' Set Project Name
#'
#' This function allows you to set the  project name. This will overwrite the
#' current value if exists.
#'
#' @param ProjectName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options ProjectName ProjData
#' @export
#'
SetProjectName <- function(ProjectName){
  if(!is.character(ProjectName)) stop('Project Name must be a character string')
  if(length(ProjectName) > 1) {
    warning('Only First String is Used')
    ProjectName <- ProjectName[1]
  }
  SetProjectData('ProjectName', ProjectName)
  return(paste('The Project name has been changed to', ProjectName))
}

#' Set PI Name
#'
#' This function allows you to set the Project's PI. This will overwrite the
#' current value if exists.
#'
#' @param PI A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options PI ProjData
#' @export
#'
SetProjectPI <- function(PI){
  if(!is.character(PI)) stop('PI Name must be a character string')
  if(length(PI) > 1) {
    warning('Only First String is Used')
    PI <- PI[1]
  }
  SetProjectData('PI', PI)
  return(paste('The Project PI has been changed to', PI))
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

ProjectAnalyst <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('analyst' %in% names(ProjData)) return(ProjData$analyst)
  }
  if(file.exists(file.path('../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../.ProjData/Data.dcf'), all = T)
    if('analyst' %in% names(ProjData)) return(ProjData$analyst)
  }
  if(file.exists(file.path('../../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../../.ProjData/Data.dcf'), all = T)
    if('analyst' %in% names(ProjData)) return(ProjData$analyst)
  }
  if(!is.null(getOption('CIDAtools.analyst'))){
    return(getOption('CIDAtools.analyst'))
  }
  return('')
}

#' Get Project Name
#'
#' This function returns the Project Name or blank if none exists.
#'
#' @return A character string with the project name
#' @keywords options ProjData ProjectName
#' @export
#'

ProjectName <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('ProjectName' %in% names(ProjData)) return(ProjData$ProjectName)
  }

  if(file.exists(file.path('../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../.ProjData/Data.dcf'), all = T)
    if('ProjectName' %in% names(ProjData)) return(ProjData$ProjectName)
  }
  if(file.exists(file.path('../../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../../.ProjData/Data.dcf'), all = T)
    if('ProjectName' %in% names(ProjData)) return(ProjData$ProjectName)
  }
  return('')
}

#' Get PI Name
#'
#' This function returns the PI Name or blank if none exists.
#'
#' @return A character string with the PI name
#' @keywords options ProjData PI
#' @export
#'

ProjectPI <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('PI' %in% names(ProjData)) return(ProjData$PI)
  }
  if(file.exists(file.path('../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../.ProjData/Data.dcf'), all = T)
    if('PI' %in% names(ProjData)) return(ProjData$PI)
  }
  if(file.exists(file.path('../../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../../.ProjData/Data.dcf'), all = T)
    if('PI' %in% names(ProjData)) return(ProjData$PI)
  }


  return('')
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

ProjectLocation <- function(path = ''){

  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('datalocation' %in% names(ProjData)){
      temp_path <- CIDA_drive_path(ProjData$datalocation)
      return(file.path(temp_path, path))
    }
  }

  if(file.exists(file.path('../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../.ProjData/Data.dcf'), all = T)
    if('datalocation' %in% names(ProjData)){
      temp_path <- CIDA_drive_path(ProjData$datalocation)
      return(file.path(temp_path, path))
    }
  }
  if(file.exists(file.path('../../.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('../../.ProjData/Data.dcf'), all = T)
    if('datalocation' %in% names(ProjData)){
      temp_path <- CIDA_drive_path(ProjData$datalocation)
      return(file.path(temp_path, path))
    }
  }

  message('Project location not found, use SetProjectData("datalocation", x).')
  return("")
}

#' Set data for project
#'
#' Allows you to set misc project data parameters
#' for Project Name, Analyst, or PI recommend you use specific function
#'
#' @param Parameter Project Parameter to be set
#' @param Value Value to set to project parameter
#' @export
#'
#'

SetProjectData <- function(Parameter, Value){
  if (!is.character(Parameter) | !is.character(Parameter))
    stop('Parameter must be a character string of length one')
  if(!is.character(Value)) stop('Value must be a character string')
  if(length(Value) > 1) {
    warning('Only First String is Used')
    Value <- Value[1]
  }
  if(Parameter=='datalocation'){
    Value <- proj.location.handler(Value)
  }
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
  } else{
    dir.create(paste0('.ProjData/'), recursive = T, showWarnings = F)
    ProjData <- list()
  }
  ProjData[Parameter] <- Value
  write.dcf(ProjData, file.path('.ProjData/Data.dcf'))
}
