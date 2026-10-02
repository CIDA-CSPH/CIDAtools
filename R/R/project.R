#' Function to act as a setter for CIDAProject properties.
setter_with_warn <- function(self, name, value) {
  # This warning will appear if users attempt to set a value directly on 
  # the CIDAProject object.
  print_warning(
    glue::glue("Setting a property on the CIDAProject object does not", 
               " update the project.json file!",
               "\nTo update the project.json, use the 'set_{name}()' function. "
    )
  )
  # Set the value
  S7::prop(self, name) <- value
  # Return new object
  return(self)
}


#' Class to encapsulate a CIDA project.
#' 
#' @param project_name The name for the current project.
#' @param principal_investigator The principal investigator for the project.
#' @param data_location The location of the data for the project.
#' @param git_location The location of git remote for the project.
#' @param analyst The analyst for the project. 
#' @param metadata User-specified metadata, can be used to customize templates
#'  with extra information not stored in the provided fields.
#' @importFrom S7 new_class class_character new_property
#' @export
CIDAProject <- S7::new_class(
  "CIDAProject",
  properties=list(
    project_name = S7::new_property(
      NULL | S7::class_character, 
      getter=function(self) { self@project_name }, 
      setter=function(self, value) { 
        setter_with_warn(self=self, name="project_name", value=value)
      }
    ),
    principal_investigator = S7::new_property(
      NULL | S7::class_character,
      getter=function(self) { self@principal_investigator }, 
      setter=function(self, value) { 
        setter_with_warn(self=self, name="principal_investigator", value=value)
      }
    ),
    data_location = S7::new_property(
      NULL | S7::class_character,
      getter=function(self) { self@data_location }, 
      setter=function(self, value) { 
        setter_with_warn(self=self, name="data_location", value=value)
      }
    ),
    git_location = S7::new_property(
      NULL | S7::class_character,
      getter=function(self) { self@git_location }, 
      setter=function(self, value) {
        setter_with_warn(self=self, name="git_location", value=value)
      }
    ),
    analyst = S7::new_property(
      NULL | S7::class_character,
      getter=function(self) { self@analyst }, 
      setter=function(self, value) { 
        setter_with_warn(self=self, name="analyst", value=value)
      }
    ),
    metadata = S7::new_property(
      NULL | S7::class_list,
      getter=function(self) { self@metadata }, 
      setter=function(self, value) { 
        setter_with_warn(self=self, name="metadata", value=value)
      }
    )
  )
)

#' Function to convert the CIDAProject class to a list (for JSON serialization)
S7::method(as.list, CIDAProject) <- function(x, ...) {
  # Get all property names
  all_prop_names <- S7::prop_names(x)
  # Return a list of all 'public' properties.
  return(setNames(lapply(all_prop_names, function(name) { S7::prop(x, name) }), all_prop_names))
}

#' Function to load a project.json file into a CIDAProject object.
#' 
#' This is an internal function which shouldn't be called by users directly.
#' @param path The path to the project JSON file.
read_raw_metadata <- function(path) {
  # Read the metadata value from file.
  json_text <- tryCatch(
    {jsonlite::read_json(path)}, 
    error=function(e) {
      print_failure(glue::glue("Unable to read metadata from '{path}': {e}"))
      stop(e)
    }
  )
  # Construct a CIDAProject object.
  cida_project <- suppressMessages(do.call(CIDAProject, json_text))
  # Return the CIDAProject object.
  return(cida_project)
}

#' Function to write a CIDAProject object to JSON.
#' 
#' @param path Path to the metadata file to read/write.
#' @param metadata The CIDAProject object to write to file.
write_raw_metadata <- function(path, metadata) {
  tryCatch(
    {metadata_list <- as.list(metadata)
    jsonlite::write_json(metadata_list, path)},
    error=function(e) {
      print_failure(glue::glue("Unable to write metadata to '{path}': {e}"))
      stop(e)
    })
}

#' Function to update a metadata entry, and write to file.
#' 
#' This is an internal function which shouldn't be called by users directly.
#' @param path Path to metadata file to read and write.
#' @param key The name/key of the metadata entry to be updated.
#' @param value The value of the the metadata entry to be updated.
update_raw_metadata <- function(path, key, value) {
  # Read the existing metadata from file.
  exist_meta <- read_raw_metadata(path)
  
  # Set the property (which validates type)
  S7::prop(exist_meta, key) <- value
  
  # Write the metadata to file
  write_raw_metadata(path, metadata)
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
    project_path <- fs::path_join(c(project_root, CIDA_DIRECTORY_NAME))
    # Check if path exists and is a folder.
    if(fs::dir_exists(project_path)){
      config_path <- fs::path_join(c(project_path, CIDA_PROJECT_CONFIG_NAME))
      # If the config file exists, parse JSON and return the object.
      if(fs::file_exists(config_path)){
        return(read_raw_metadata(config_path))
      }
    }
    project_root <- fs::path_dir(project_root)
  }
  return(NULL)
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
    print_success(glue::glue("Created new project directory at {project_root}."))
  } else if (!is.null(current_project(project_root = project_root))) {
    print_warning(glue::glue("A CIDA project already exists at {project_root}."))
  } else if (fs::is_file(project_root)) {
    print_error(glue::glue("The path '{project_root}' exists, but is a file!"))
  } else{
    print_info("Project directory already exists.")
  }
  
  # Build a new project config
  project_config_dir <- fs::path_join(c(project_root, CIDA_DIRECTORY_NAME))
  
  # Create the project config directory
  dir.create(project_config_dir, recursive=T, showWarnings = F)
  
  # Create the new CIDAProject object
  project_metadata <- CIDAProject(
    project_name = project_name,
    principal_investigator = principal_investigator,
    analyst = analyst,
    data_location = data_location,
    git_location = git_location
  )
  
  # Write the config to file.
  project_metadata_path <- fs::path_join(c(project_config_dir, CIDA_PROJECT_CONFIG_NAME))
  write_raw_metadata(path = project_metadata_path, metadata = project_metadata)
  
  # Create the main README file.
  
  
  
}

#' Writes a templated README to a project folder
write_templated_readme <- function(project_root, template_name, metadata, overwrite=FALSE, subdir=NULL) {
  # Get absolute path
  project_root <- fs::path_abs(project_root)
  
  # If the project root is not a directory, stop here.
  if(!fs::is_dir(project_root)) {
    print_failure(glue::glue("project_root {project_root} is not a directory, ensure directory exists."))
    return(NULL)
  }
  
  # Construct the path to the README
  if(!is.null(subdir)) {
    subdir_path <- fs::path_join(c(project_root, subdir))
    dir.create(subdir_path, recursive = T, showWarnings = F)
    readme_path <- fs::path_join(c(subdir_path, "README.md"))
  } else{
    readme_path <- fs::path_join(c(project_root, "README.md"))
  }
  
  # Only make the README if it doesn't already exist, or if we specified to overwrite
  if(overwrite || ((!fs::file_exists(readme_path)) && (fs::is_file(readme_path)))) {
    # Obtain the template file using the provided template name.
    template_str <- system.file("extdata/templates/readme", template_name, package="CIDAtools")
    # Populate the template with information from the CIDAProject metadata.
    rendered_template_string <- rlang::inject(jinjar::render(template_str, !!!as.list(metadata)))
    
    # Write the rendered template to file.
    write(rendered_template_string, readme_path)
  } else{
    rel_readme_path <- fs::path_rel(readme_path, start=project_root)
    print_failure(glue::glue("{rel_readme_path} already exists, will not overwrite."))
  }
  
  #TODO: Finish me
}
