#' @include persistence.R
NULL

#' Gets the path to the defaults file.
#' 
#' @description
#' This is a function here because of R's compile-time evaluation of package-level
#' variables.
#' 
get_defaults_path <- function() {
  return(fs::path_home(CIDA_DIRECTORY_NAME, "project_defaults.json"))
}

#' Creates the defaults file
#' 
init_defaults <- function(defaults_path = NULL) {
  # Get the default path if none is supplied.
  defaults_path <- ifelse(is.null(defaults_path), get_defaults_path(), defaults_path)
  # Create defaults file if none exists
  if(!fs::file_exists(defaults_path)){
    # Create defaults directory if none exists.
    defaults_dir <- fs::path_dir(defaults_path)
    dir.create(defaults_dir, showWarnings = T, recursive = T, mode = "0700")
    # Create new model
    new_model <- CIDADefaultsModel()
    # Write the defaults files
    write_model(defaults_path, new_model)
  }
}


#' The model for CIDA defaults.
CIDADefaultsModel <- S7::new_class(
  "CIDADefaultsModel",
  properties = list(
    analyst = S7::new_property(NULL | S7::class_character),
    github_host = S7::new_property(S7::class_character, default="github.com"),
    github_protocol = S7::new_property(S7::class_character, default="https"),
    github_username = S7::new_property(NULL | S7::class_character),
    github_password = S7::new_property(NULL | S7::class_character)
  ),
  validator=function(self){
    # These properties can be NULL or a string
    scalar_props <- c("analyst", "github_username", "github_password")
    for(scalar_prop in scalar_props) {
      prop_val <- S7::prop(self, scalar_prop)
      prop_len <- length(prop_val)
      if((!is.null(prop_val)) && (prop_len != 1)){
        return(glue::glue("@{scalar_prop} must be NULL or scalar, but has length {prop_len}."))
      }
    }
    # These properties can be string, but cannot be NULL.
    scalar_non_missing_props <- c("github_host", "github_protocol")
    for(scalar_non_missing_prop in scalar_non_missing_props){
      prop_val <- S7::prop(self, scalar_non_missing_prop)
      prop_len <- length(prop_val)
      if(prop_len != 1){
        return(glue::glue("@{scalar_non_missing_prop} must be scalar, but has length {prop_len}."))
      }
    }
    
    return(NULL)
  }
)

#' Function to convert the CIDADefaultsModel class to a list (for JSON serialization)
S7::method(as.list, CIDADefaultsModel) <- function(x, ...) {
  # Get all property names
  all_prop_names <- S7::prop_names(x)
  # Return a list of all 'public' properties.
  return(
    setNames(
      lapply(
        all_prop_names,
        function(name) {
          tmp_v <- S7::prop(x, name)
          if(is.null(tmp_v)){
            return(NA)
          }
          return(tmp_v)
        }
      ),
      all_prop_names
    )
  )
}


#' Creates a wrapper around CIDA defaults
CIDADefaults <- S7::new_class(
  "CIDADefaults",
  properties = list(
    path = S7::new_property(S7::class_character),
    .getter = S7::new_property(S7::class_function),
    .setter = S7::new_property(S7::class_function),
    .par = S7::new_property(NULL),
    analyst = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("analyst"),
      setter = make_setter("analyst")
    ),
    github_host = S7::new_property(
      S7::class_character, 
      getter = make_getter("github_host"),
      setter = make_setter("github_host")
    ),
    github_protocol = S7::new_property(
      S7::class_character, 
      getter = make_getter("github_protocol"),
      setter = make_setter("github_protocol")
    ),
    github_username = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("github_username"),
      setter = make_setter("github_username")
    ),
    github_password = S7::new_property(
      NULL | S7::class_character,
      getter = make_getter("github_password"),
      setter = make_setter("github_password")
    )
  ),
  constructor = function(path=NULL){
    # TODO: ???
    path
    # Get the default path if none is supplied
    path <- ifelse(is.null(path), get_defaults_path(), path)
    # Initialize the defaults if needed
    init_defaults(defaults_path=path)
    # Create cache helper and unpack functions
    gs_list <- cache_helper(path, CIDADefaultsModel)
    getter <- gs_list$getter
    setter <- gs_list$setter
    model <- gs_list$model
    S7::new_object(
      .parent=S7::S7_object(), 
      path=path, 
      .getter=getter, 
      .setter=setter,
      .par=NULL,
      analyst=model@analyst,
      github_host=model@github_host,
      github_protocol=model@github_protocol,
      github_username=model@github_username,
      github_password=model@github_password
    )
  }
)
