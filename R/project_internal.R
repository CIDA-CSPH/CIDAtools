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


#' Internal Function to return Project Data path for use in the other methods
#' that read .ProjData/Data.dcf
#'
#' @noMd
#' @noRd
#'
get_project_data_dir <- function(){
  path <- ""

  ## TODO There should be a way to find the top project directory and not use
  #       the ../ relative navigation below that will fail after 3 subfolders.
  if(fs::dir_exists(path='.ProjData/')){
    path <- '.ProjData/'
  }else if(fs::dir_exists(path='../.ProjData/')){
    path <- '../.ProjData/'
  }else if(fs::dir_exists(path='../../.ProjData/')){
    path <- '../../.ProjData/'
  }else if(fs::dir_exists(path='../../../.ProjData/')){
    path <- '../../../.ProjData/'
  }else{
    warning(".ProjData directory not found in project.",call.=FALSE,immediate. = TRUE)
  }
  return(path)
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


#' Internal Function to save Project Data for use in the other methods
#' that update values in  .ProjData/Data.dcf
#'
#' @noMd
#' @noRd
#'
save_project_data <- function(project_metadata){
  to_save <- NULL
  path <- get_project_data_dir()
  print(paste("Path",path))

  current_meta_data <- get_project_meta_data()

  if(is.null(path)|| path==""){
    path <- '.ProjData/'
    if(!fs::dir_exists(path)){
      dir.create(paste0('.ProjData/'), recursive = T, showWarnings = F)
    }
  }else{
    directory=fs::path_dir(path )
    if(!fs::dir_exists(directory)){
      dir.create(paste0('.ProjData/'), recursive = T, showWarnings = F)
    }
  }

  if(!is.null(current_meta_data) ){
    to_save <- current_meta_data
    for (element in names(project_metadata)) {
      to_save[element]=project_metadata[element]
    }
  }else{
    to_save <- project_metadata
  }
  dcfFile <- paste(path,"/Data.dcf", sep="")
  write_project_data(to_save,dcfFile)
}


#' Internal Function to write Project Data .ProjData/Data.dcf
#'
#' @param project_metadata project metadata to save
#' @param path path to save the data to.
#'
#' @noMd
#' @noRd
#'
write_project_data <- function(project_metadata,path){
  write.dcf(project_metadata, fs::path(path))
  return(TRUE)
}
