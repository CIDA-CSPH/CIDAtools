read_raw_metadata <- function(path) {
  # Read the metadata value from file.
  json_text <- tryCatch(
    {jsonlite::read_json(path)}, 
    error=function(e) {
      print_failure(glue::glue("Unable to retrieve value: {e}"))
      stop(e)
    }
  )
  # Return the value
  return(json_text)
}

#' Function to retrieve a metadata value from file.
#' 
retrieve_metadata_value <- function(self, name) {
  message("reading file")
  # Read the metadata value from file.
  json_text <- read_raw_metadata(self@.path)
  
  # Try to extract the property associated with the name.
  result_val <- tryCatch(
    {json_text[[name]]}, 
    error=function(e) {
      print_error(glue::glue("{self} has no attribute '{name}'"))
      stop(e)
    }
  )
  
  return(result_val)
}

#' Function to set a metadata file in the file.
#' 
set_metadata_value <- function(self, name, value) {
  message("writing file.")
  # Read the metadata value from file.
  json_text <- read_raw_metadata(self@.path)
  
  # Check if the name of the value we're trying to set is a valid property name.
  if(S7::prop_exists(self, name)){
    # Set the value
    json_text[[name]] = value
  } else {
    # Error if it is not a valid name.
    stop(glue::glue("{self} has no property '{value}'."))
  }
  
  # Write the file, overwriting existing contents.
  jsonlite::write_json(as.list(self), self@.path, auto_unbox=T, null="null")
  
  return(self)
}

#' Class to encapsulate a CIDA project.
#' 
#' @param path The path to the project JSON file
#' @param project_name The name for the current project.
#' @param principal_investigator The principal investigator for the project.
#' @param data_location The location of the data for the project.
#' @param git_location The location of git remote for the project.
#' @param analyst The analyst for the project. 
#' @importFrom S7 new_class class_character new_property
#' @export
CIDAProject <- S7::new_class(
  "CIDAProject",
  properties=list(
    .path = S7::class_character,
    project_name = S7::new_property(
        NULL | S7::class_character,
        getter = function(self) { retrieve_metadata_value(self=self, name="project_name") },
        setter = function(self, value) { set_metadata_value(self=self, name="project_name", value=value) }
    ),
    principal_investigator = S7::new_property(
        NULL | S7::class_character,
        getter=function(self) { retrieve_metadata_value(self, name="principal_investigator") },
        setter = function(self, value) { set_metadata_value(self=self, name="principal_investigator", value=value) }
    ),
    data_location = S7::new_property(
        NULL | S7::class_character,
        getter=function(self) { retrieve_metadata_value(self, name="data_location") },
        setter = function(self, value) { set_metadata_value(self=self, name="data_location", value=value) }
    ),
    git_location = S7::new_property(
        NULL | S7::class_character,
        getter=function(self) { retrieve_metadata_value(self, name="git_location") },
        setter = function(self, value) { set_metadata_value(self=self, name="git_location", value=value) }
    ),
    analyst = S7::new_property(
        NULL | S7::class_character,
        getter=function(self) { retrieve_metadata_value(self, name="analyst") },
        setter = function(self, value) { set_metadata_value(self=self, name="analyst", value=value) }
    )
  )
)

#' Function to convert the CIDAProject class to a list (for JSON serialization)
S7::method(as.list, CIDAProject) <- function(x, ...) {
  # Get all property names
  all_prop_names <- S7::prop_names(x)
  # Filter to just names which don't start with '.' (our arbitrary marker for a
  # private property).
  public_prop_names <- all_prop_names[which(!startsWith(all_prop_names, "."))]
  # Return a list of all 'public' properties.
  return(setNames(lapply(public_prop_names, function(name) { retrieve_metadata_value(self=x, name=name) }), public_prop_names))
}

#' Retrieve the currently active CIDA project.
#'
#' This function will recursively search for a CIDA project config file, starting
#' with the current working directory and traversing upwards toward the root directory.
#' 
#' @param project_root The root directory for the project, or NULL. If the 
#'  root directory is not specified, uses getwd() to obtain the current working
#'  directory.
#' @cidatools current_project project
#' @export
current_project <- function(project_root = NULL) {
  # If the project path is not specified, use the working directory.
  project_root <- fs::path_abs(ifelse(is.null(project_root), getwd(), project_root))
  
  # If the project root is not a directory, error out.
  if(!fs::is_dir(project_root)) {
    print_failure(glue::glue("Path {project_root} is not a directory."))
    return(NULL)
  }
  
  # Quit when we reach the root directory.
  while(project_root != fs::path_dir(project_root)) {
    # Check the current directory for a CIDA folder.
    project_path <- fs::path_join(project_path, CIDA_DIRECTORY_NAME)
    # Check if path exists and is a folder.
    if(fs::dir_exists(project_path)){
      config_path <- fs::path_join(project_path, CIDA_PROJECT_CONFIG_NAME)
      # If the config file exists, parse JSON and return the object.
      if(fs::file_exists(config_path)){
        
      }
    }
  }
  
}

#'Create Project Directory + readme files
#'
#'This function creates the standard project organization structure for CIDA
#'within a folder that already exists.
#'
#'@param path Where should they be created? Default is the working directory.
#'@param template Which subdirectories to create
#'@param project_name Name of project, (required)
#'@param pi Name of PI and credentials, or "" for blank
#'@param analyst Name of Analyst(s), (required)
#'@param data_location Location of project on CIDA Drive, or "" for blank
#'@param git_location Location project on GitHub
#'@return This function creates the desired project subdirectories and readmes,
#'  as well as a standard .gitignore file files. It will not overwrite the file
#'  however if it does not exist. It does not return anything.
#'@keywords project createproject
#'
#'@seealso proj_setup() is the internal wrapper for this that gets called when
#'  using the RStudio GUI to create a project
#'
#'@cidatools create_project project
#'@export
create_project <- function(
      project_root = NULL,
      project_name = NULL, 
      pi = NULL, 
      analyst = NULL, 
      data_location = NULL,
      git_location = NULL
    ){
  # If the project name does not exist, use a placeholder
  project_name <- ifelse(is.null(project_name), "CIDAProject", project_name)
  
  # If the project path is not specified, use the working directory.
  project_root <- fs::path_abs(ifelse(is.null(project_root), getwd(), project_root))
  
  # If the project path does not exist, create it.
  if(!fs::file_exists(project_root)) {
    dir.create(project_root, recursive=T)
  }

  
}
