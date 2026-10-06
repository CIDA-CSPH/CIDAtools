#' @include persistence.R
NULL

#' Class to encapsulate a CIDA project.
#' 
#' @description
#' This function acts as a wrapper for a CIDA project, allowing reading/writing
#' of metadata values. 
#'
#' @param project_name The name for the current project.
#' @param principal_investigator The principal investigator for the project.
#' @param data_location The location of the data for the project.
#' @param git_location The location of git remote for the project.
#' @param analyst The analyst for the project. 
#' @param metadata User-specified metadata, can be used to customize templates
#'  with extra information not stored in the provided fields.
#' @export
CIDAProject <- S7::new_class(
  "CIDAProject",
  properties=list(
    .getter = S7::new_property(S7::class_function),
    .setter = S7::new_property(S7::class_function),
    defaults = S7::new_property(CIDADefaults),
    path = S7::new_property(S7::class_character),
    project_name = S7::new_property(
        NULL | S7::class_character,
        getter = make_getter("project_name", default_name="defaults"),
        setter = make_setter("project_name")
    ),
    principal_investigator = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("principal_investigator", default_name="defaults"),
      setter = make_setter("principal_investigator")
    ),
    data_location = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("data_location"),
      setter = make_setter("data_location")
      ),
    git_location = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("git_location", default_name="defaults"),
      setter = make_setter("git_location")
      ),
    analyst = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("analyst", default_name="defaults"),
      setter = make_setter("analyst")
      ),
    metadata = S7::new_property(
      NULL | S7::class_list,
      getter = make_getter("metadata", default_name="defaults"),
      setter = make_setter("metadata")
      )
  ),
  constructor=function(path){
    # TODO: do we need this?
    path
    # Create cache helper and unpack functions
    gs_list <- cache_helper(path, CIDAProjectModel)
    getter <- gs_list$getter
    setter <- gs_list$setter
    model <- gs_list$model
    # Return the constructed object
    S7::new_object(
      .parent=S7::S7_object(), 
      path=path, 
      .getter=getter, 
      .setter=setter,
      defaults=CIDADefaults(),
      project_name=model@project_name,
      principal_investigator=model@principal_investigator,
      data_location=model@data_location,
      git_location=model@git_location,
      analyst=model@analyst,
      metadata=model@metadata
    )
  }
  
)


#' Class to encapsulate a CIDA project.
#' 
#' @param project_name The name for the current project.
#' @param principal_investigator The principal investigator for the project.
#' @param data_location The location of the data for the project.
#' @param git_location The location of git remote for the project.
#' @param analyst The analyst for the project. 
#' @param metadata User-specified metadata, can be used to customize templates
#'  with extra information not stored in the provided fields.
CIDAProjectModel <- S7::new_class(
  "CIDAProjectModel",
  properties=list(
    project_name = S7::new_property(NULL | S7::class_character),
    principal_investigator = S7::new_property(NULL | S7::class_character),
    data_location = S7::new_property(NULL | S7::class_character),
    git_location = S7::new_property(NULL | S7::class_character),
    analyst = S7::new_property(NULL | S7::class_character),
    metadata = S7::new_property(NULL | S7::class_list)
  ),
  validator=function(self){
    scalar_props <- c("project_name", "principal_investigator", "data_location", "git_location")
    for(scalar_prop in scalar_props) {
      prop_val <- S7::prop(self, scalar_prop)
      prop_len <- length(prop_val)
      if((!is.null(prop_val)) && (prop_len != 1)){
        return(glue::glue("@{scalar_prop} must be NULL or scalar, but has length {prop_len}."))
      }
    }
    return(NULL)
  }
)

#' Function to convert the CIDAProject class to a list (for JSON serialization)
S7::method(as.list, CIDAProjectModel) <- function(x, ...) {
  # Get all property names
  all_prop_names <- S7::prop_names(x)
  # Return a list of all 'public' properties.
  return(setNames(lapply(all_prop_names, function(name) { S7::prop(x, name) }), all_prop_names))
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
        return(read_model(config_path, CIDAProjectModel))
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
#'@param folders_to_create A list of the project subdirectories to create.
#'  If NULL, uses the value of 'CIDA_PROJECT_DEFAULT_FOLDERS'.
#'@return Function to create a CIDA project locally. This will create the 
#'  CIDA project structure with subdirectories, .gitignore, READMEs, etc, but
#'  will not replace existing files. 
#'@keywords project create_project
#'
#'
#'@cidatools create_project project
#'@export
create_project <- function(
      project_root = NULL,
      project_name = NULL, 
      principal_investigator = NULL, 
      analyst = NULL, 
      data_location = NULL,
      git_location = NULL,
      folders_to_create=NULL
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
  
  # Create the new CIDAProjectModel object
  project_metadata <- CIDAProjectModel(
    project_name = project_name,
    principal_investigator = principal_investigator,
    analyst = analyst,
    data_location = data_location,
    git_location = git_location
  )
  
  # Write the config to file.
  project_metadata_path <- fs::path_join(c(project_config_dir, CIDA_PROJECT_CONFIG_NAME))
  write_model(path=project_metadata_path, instance=project_metadata)
  
  # Create the main README file.
  write_templated_readme(project_root=project_root, template_name="Project.md", metadata=project_metadata)
  
  # Create the other directories and populate with READMEs
  if(is.null(folders_to_create)){
    project_subdirs <- CIDA_PROJECT_DEFAULT_FOLDERS
  }else{
    project_subdirs <- folders_to_create
  }
  
  for(project_subdir in project_subdirs) {
    # Get the path to this subdirectory.
    project_subdir_path <- fs::path_join(c(project_root, project_subdir))
    # If the subdirectory does not already exist, create it.
    if(!fs::file_exists(project_subdir_path)) {
      dir.create(project_subdir_path, recursive = T, showWarnings = F)
    }
    # Write the README to this subdirectory.
    write_templated_readme(
      project_root = project_root,
      template_name = glue::glue("{project_subdir}.md"),
      subdir = project_subdir,
      metadata = project_metadata
    )
  }
  
  # Create the Rprofile hook 
  # TODO: No.
  
  # Create default Rproj.
  write_rproj(project_root=project_root, metadata=project_metadata)
  
  # Create default .gitignore
  write_gitignore(project_root=project_root)
  
  # Return the created project metadata
  return(project_metadata)
}

#' Obtains a path to the config file.
ensure_config_path <- function(project_root) {
  project_root <- fs::path_abs(project_root)
  if((fs::path_file(project_root) == CIDA_PROJECT_CONFIG_NAME) && (fs::path_file(fs::path_dir(project_root)) == CIDA_DIRECTORY_NAME)){
    config_path <- project_root
  } else{
    config_path <- fs::path_join(c(project_root, CIDA_DIRECTORY_NAME, CIDA_PROJECT_CONFIG_NAME))
  }
  
  return(config_path)
}


#' Writes a CIDAProjectModel object to JSON
write_config <- function(project_root, metadata, overwrite=FALSE) {
  
  # Construct a config path from a path to the project root.
  config_path <- ensure_config_path(project_root = project_root)
  
  # Create the file only if it doesn't exist OR we are forcing an overwrite.
  if(overwrite || !(fs::file_exists(config_path) && fs::is_file(config_path))){
    # Create the enclosing directory if it doesn't exist
    config_dir <- fs::path_dir(config_path)
    dir.create(config_dir, showWarnings = T, recursive = T)
    # Serialize the model to JSON
    write_model(path=config_path, instance=metadata)
    print_success(glue::glue("Created new project JSON at {config_path}"))
  }else{
    print_failure(glue::glue("CIDA project config already exists at {config_path}, will not overwrite."))
  }
}

#' Creates a .gitignore file at the given path if none exists.
write_gitignore <- function(project_root) {
  
  if((!fs::file_exists(project_root)) || (!fs::is_dir(project_root))){
    print_failure(glue::glue("project_root {project_root} is not a directory, ensure directory exists."))
  }
  # Generate gitignore path
  gitignore_path <- fs::path_join(c(project_root, ".gitignore"))
  # Relative path for printing.
  gitignore_rel_path <- fs::path_rel(gitignore_path, start=project_root)
  
  # If path already exists, don't create a new one.
  if(fs::file_exists(gitignore_path) && fs::is_file(gitignore_path)) {
    print_failure(glue::glue("{gitignore_rel_path} already exists, will not overwrite."))
  }else{
    # Template gitignore
    default_gitignore <- system.file("extdata", "gitignore", package="CIDAtools", mustWork = T)
    # Create the gitignore file.
    fs::file_copy(default_gitignore, gitignore_path)
    # Print success
    print_success(glue::glue("Created {gitignore_rel_path}."))
  }
}


#' Creates an .Rproj file in the current directory if none exists.
write_rproj <- function(project_root, metadata) {
  
  if((!fs::file_exists(project_root)) || (!fs::is_dir(project_root))){
    print_failure(glue::glue("project_root {project_root} is not a directory, ensure directory exists."))
  }
  
  #TODO: check for *any* .Rproj file, not just one matching the name from the current metadata.
  
  # Generate the name for the project based on the project name, or use a default if not available
  rproj_name <- ifelse(is.null(metadata@project_name), "CIDAProject", metadata@project_name)
  rproj_path <- fs::path_join(c(project_root, glue::glue("{rproj_name}.Rproj")))
  
  # Path to .Rproj, relative to project root directory.
  rproj_relative <- fs::path_rel(rproj_path, start=project_root)
  
  if(fs::file_exists(rproj_path) && fs::is_file(rproj_path)){
    print_failure(glue::glue("{rproj_relative} already exists, will not create new .Rproj."))    
  }else {
    # The template file for the CIDA project.
    default_rproj <- system.file("extdata", "DefaultCIDAProject.Rproj", package = "CIDAtools", mustWork = T)
    fs::file_copy(default_rproj, rproj_path)
    # Print success
    print_success(glue::glue("Created {rproj_relative}."))
  }
  
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
  if(overwrite || !(fs::file_exists(readme_path) && fs::is_file(readme_path))) {
    # Obtain the template file using the provided template name.
    template_str <- readr::read_file(system.file("extdata/templates/readme", template_name, package="CIDAtools", mustWork = T))
    # Populate the template with information from the CIDAProject metadata.
    rendered_template_string <- jinjar::render(template_str, config=as.list(metadata))
    # Write the rendered template to file.
    write(rendered_template_string, readme_path)
    # Print success message
    readme_path_rel <- fs::path_rel(readme_path, start=project_root)
    print_success(glue::glue("Created README.md at {readme_path_rel}"))
  } else{
    rel_readme_path <- fs::path_rel(readme_path, start=project_root)
    print_failure(glue::glue("{rel_readme_path} already exists, will not overwrite."))
  }
}

create_github_project <- function(
    project_name = NULL,
    repository_name = NULL, 
    project_root = NULL, 
    template_name = NULL, 
    principal_investigator = NULL, 
    analyst = NULL, 
    data_location = NULL, 
    description = NULL, 
    visibility = "internal"
){
  
}
