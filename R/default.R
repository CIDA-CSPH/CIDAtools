#' Set Global Default Analyst Value
#'
#'
#' This function allows you to set the default analyst at the user level and
#' if possible changes the default analyst in the template for new projects.
#'
#' @param analyst_name A string containing the analyst name
#' @param update_template Bool whether or not to update analyst in the project template.
#' @return A message stating the name has been changed.
#' @keywords options Analyst
#' @export
#'
set_global_default_analyst <- function(analyst_name,update_template=FALSE){
  analyst_name <- check_string_param_value(analyst_name,'analyst_name')
  # Save to user cida_defaults.dcf
  set_default_value('analyst_name',analyst_name)

  # Save to project templates
  if(update_template){
    set_template_analyst(analyst_name)
  }
  return(paste('The default analyst name has been changed to',
               analyst_name))
}

#' Get Default Analyst Value
#'
#'
#' This function allows you to get the default analyst.
#'
#' @return The default analyst or an empty string if not set.
#' @keywords options Analyst
#' @export
#'
get_global_default_analyst <- function(){
  value <- get_default_value('analyst_name')
  return(value)
}



#' Remove Default Analyst from ~/cida_defaults.dcf
#'
#' This function removes the default analyst set with set_global_default_analyst()
#'  from the users ~/cida_defaults.dcf.
#'
#'
#' @return Message indicating success or failure
#' @keywords Analyst remove
#' @export
#'
#'
remove_global_default_analyst <- function(){
  to_save <- NULL
  defaults <- read_global_defaults()
  msg <- ""
  if(!is.null(defaults)){
    to_save <- list()
    for (element in names(defaults)) {
      if(element!="analyst_name"){
        to_save[element]=defaults[element]
      }
    }
    write_global_defaults(to_save)
    msg <- paste('The default analyst has been removed.')
  }else{
    warning("Analyst not removed: global default file not found.")
    msg <- paste("Analyst not removed: global default file not found.")
  }
  return(msg)
}



#' Sets a default drive path in ~/cida_defaults.dcf
#'
#' This function sets a default path at the user level to access the project
#' drive. If the drive is not detected automatically this file will be used to
#' set a default path if set.
#'
#' @param path Path to the main project(CIDA) drive
#'
#' @return Message that path was set.
#' @export
#'
set_global_default_path <- function(path=""){
  path <- check_string_param_value(path,'global_default_path')
  set_default_value('path',path)
  return(paste('The default project path has been changed to',path))
}

#' removes the default drive path in ~/cida_defaults.dcf
#'
#' This function removed the default path at the user level to access the project
#' drive.
#'
#' @export
#'
remove_global_default_path <- function(){
  set_default_value('path',"")
}


#' Get Default drive path
#'
#'
#' This function returns the user level default drive path set in ~/cida_defaults.dcf
#'
#' @return The default path
#' @keywords options path
#' @export
#'
get_global_default_path <- function(){
  path <- ''
  path <- get_default_value('path')
  return(path)
}

#' Sets a default parameter in ~/cida_defaults.dcf
#'
#' This function sets a default parameter at the user level.
#'
#' @param parameter parameter name
#' @param value new value
#'
#'
#' @export
#'
set_default_value <- function(parameter,value){
  defaults <- get_defaults()
  if(parameter %in% names(defaults)){
    defaults[parameter] <- value
  }else{
    defaults[parameter] <- value
  }
  save_global_defaults(defaults)
}

#' Get a specific user level default
#'
#'
#' This function returns a specific user level defaults set in ~/cida_defaults.dcf
#'
#' @param parameter The name of the specific parameter to lookup.
#' @return The user level defaults named parameter
#' @keywords options path
#' @export
#'
get_default_value <- function(parameter){
  default_value <- ""
  defaults <- get_defaults()
  if( parameter %in% names(defaults)){
    default_value <- defaults[parameter][1,1]
  }else{
    warning("Parameter:",parameter," does not exist in default values.\n")
  }
  return(default_value)
}


#' Get a list of all user level defaults
#'
#'
#' This function returns the user level defaults set in ~/cida_defaults.dcf
#'
#' @return The user level defaults
#' @keywords options path
#' @export
#'
get_defaults <- function(){
  default_values <- NULL

  tmp <- read_global_defaults()
  if(!is.null(tmp)){
    default_values <- tmp
  }
  return(default_values)
}

#' Get CIDA Defaults read from ~/cida_defaults.dcf
#' Use fs to get a cross platform path to the user directory to store
#' cida_defaults.dcf then read cida_defaults.dcf and return the object.
#'
#' @returns the default object stored in cida_defaults.dcf
#'
#' @noRd
#' @noMd
#'
read_global_defaults <- function(){
  default <- NULL

  home_dir <- fs::path_home()

  path <-fs::path_join(c(home_dir,"/cida_defaults.dcf"))

  if(file.exists(file.path(path))){
    default <- read.dcf(file.path(path), all = T)
  }else{
    warning("~/cida_defaults.dcf at full path:",path,"\n does not exist.\nNothing was loaded.")
  }

  return(default)
}

#' Write global defaults to ~/cida_defaults.dcf
#' Use fs to get a cross platform path to the user directory to store
#' cida_defaults.dcf then write cida_defaults.dcf.
#'
#' @returns bool for success or failure
#'
#' @noRd
#' @noMd
#'

write_global_defaults <- function( defaults_to_write){
  home_dir <- fs::path_home()
  path <-fs::path_join(c(home_dir,"/cida_defaults.dcf"))
  write.dcf(defaults_to_write, file.path(path))

  return(TRUE)
}

#' Save CIDA Defaults to ~/cida_defaults.dcf
#'
#' Use fs to get a cross platform path to the user directory to store
#' cida_defaults.dcf then save cida_defaults.dcf avoids inadvertent removal of
#' default values by first reading in the file, then overwriting the values provided,
#' preserving any not specified.  To remove default values use remove_global_default().
#'
#' @param new_default a list of new default values
#'
#' @noRd
#' @noMd
#'
save_global_defaults<- function(new_default){
  to_save <- NULL

  defaults <- read_global_defaults()
  if(!is.null(defaults) ){
    to_save <- defaults
    for (element in names(new_default)) {
        to_save[element]=new_default[element]
    }
  }else{
    to_save <- new_default
  }
  write_global_defaults(to_save)
}


#' Save template analyst to CIDA project template
#'
#' Save analyst to the CIDA project template for new projects.
#'
#' @param analyst_name Name of the default analyst for new projects.
#'
#' @noRd
#' @noMd
#'

set_template_analyst <- function(analyst_name=""){
  site_path = R.home(component = "home")
  project_setup <- paste0(site_path,
  '/library/CIDAtools/rstudio/',
  'templates/project/proj_setup.dcf')
  if(file.access(project_setup, 2) == -1)
    stop(paste0('You do not have permission to change\n',
                'New CIDA Project Template'))
  DCF <- read.dcf(file.path(project_setup), all = T)
  DCF$Default[DCF$Parameter == 'analyst' & !is.na(DCF$Parameter)] <- analyst_name
  write.dcf(DCF, file.path(project_setup))
}
