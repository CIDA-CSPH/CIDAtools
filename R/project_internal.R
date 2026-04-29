#'
#' get_default_path() - checks the current project remote path first then
#' project metadata file then checks user path file to return a default path to
#'  the CIDA Drive if no path was found automatically.
#'
#' @return path of project(CIDA) drive
#' @noRd
#' @noMd
#'
#'

get_default_path <- function(){
  path <- ""
  ## TODO. Check project and then check User/Global Default

  if(! is.null(options("cida_tools.remote_current_project_path")) && options("cida_tools.remote_current_project_path")!=""){
    tmpPath=as.character(options("cida_tools.remote_current_project_path"))
    if(fs::dir_exists(tmpPath)){
      path <- tmpPath
      # TODO: Fix this
      path <- sub("BRANCHES.*","",path)
    }
  }

  if(is.null(path) || path==""){
    # Attempt to load project meta data and pull the path from it.
    project_location=get_full_project_path()
    project_dir=get_project_location()


    if( (! is.null(project_location)&& project_location!="") && (! is.null(project_dir) && project_dir!="")){
      path <- find_drive_location(project_location,project_dir)
    }else if(! is.null(project_location)){
      path <- project_location
    }#else if(! is.null(project_dir)){
    #}
  }

  if(is.null(path) || path==""){
    #Attempt to load the global default path
    path <- get_global_default_path()
  }

  if(is.null(path) || path==""){
    warning("Failed to load project or global defaul location.")
  }
  return(path)
}


#' Internal Function to return Project Data path for use in the other methods
#' that read .ProjData/Data.dcf.  If options for the local/remote path are specified
#' they will supersede the current directory upwards traversal to find .ProjData.
#' If the paths are empty the normal traversal will be used that will fail after 3 parent
#' directories.
#'
#' @noMd
#' @noRd
#'
get_project_data_dir <- function(){
  path <- ""

  if(!is.null(options("cida_tools.current_project_path")) && options("cida_tools.current_project_path") !=""){
    tmpPath <- fs::path(options("cida_tools.current_project_path"))
    checkSuffix <- fs::path_join(c(tmpPath,'.ProjData/'))
    if(fs::dir_exists(checkSuffix)){
      path <- checkSuffix
    }
  }else if(!is.null(options("cida_tools.remote_current_project_path")) && options("cida_tools.remote_current_project_path") !=""){
    tmpPath <- as.character(options("cida_tools.remote_current_project_path"))
    checkSuffix <- fs::path_join(c(tmpPath,'.ProjData/'))
    if(fs::dir_exists(checkSuffix)){
      path <- checkSuffix
    }
  }

  if(path==""){
    ## TODO There should be a way to find the top project directory and not use
    #       the ../ relative navigation below that will fail after 3 subfolders.
    if(fs::dir_exists('.ProjData/')){
      path <- '.ProjData/'
    }else if(fs::dir_exists('../.ProjData/')){
      path <- '../.ProjData/'
    }else if(fs::dir_exists('../../.ProjData/')){
      path <- '../../.ProjData/'
    }else if(fs::dir_exists('../../../.ProjData/')){
      path <- '../../../.ProjData/'
    }else{
      warning(".ProjData directory not found in project.",call.=FALSE,immediate. = TRUE)
      path <- '.ProjData/'
    }
  }

  return(fs::path(path))
}



#' Internal Function to return Project Data path for use in the other methods
#' that read .ProjData/Data.dcf
#'
#' @noMd
#' @noRd
#'
get_project_data_path <- function(){
  path <- get_project_data_dir()
  path <- fs::path_join(c(path,"Data.dcf"))
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
  if(!is.null(path)){
    directory=fs::path_dir(path )
    if(!fs::dir_exists(directory)){
      fs::dir_create(path, recursive = TRUE, showWarnings = F)
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


#' helper function to cleanup project location
#' @param loc project location path to clean up
#' @noMd
#' @noRd
#'
proj_location_handler <- function(loc="") {
  loc <- gsub("/Volumes/sph-cida", "", loc)
  loc <- gsub("P:/", "", loc)
  loc <- gsub(".*BRANCHES", "BRANCHES", loc)
  loc <- gsub("/$", "", loc)
  return(loc)
}


#' Function to call to setup the package
#'
#'
#' @noMd
#' @noRd
.onLoad <- function(libname,pkgname){
  options(cida_tools.current_project_path="")
  options(cida_tools.remote_current_project_path="")
}
