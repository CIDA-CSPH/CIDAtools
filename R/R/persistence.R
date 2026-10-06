#' Function to load a project.json file into a CIDAProject object.
#' 
#' This is an internal function which shouldn't be called by users directly.
#' @param path The path to the project JSON file.
read_model <- function(path, model) {
  # Read the metadata value from file.
  json_text <- tryCatch(
    {jsonlite::read_json(path)}, 
    error=function(e) {
      print_failure(glue::glue("Unable to read {model} from {path}: {e}"))
      stop(e)
    }
  )
  # Construct a model object.
  cida_project <- do.call(model, json_text)
  # Return the model object.
  return(cida_project)
}

#' Function to write a model object to JSON.
#' 
#' @param path Path to the metadata file to read/write.
#' @param instance The instance of the model object to write.
write_model <- function(path, instance) {
  tryCatch(
    {model_list <- as.list(instance)
    jsonlite::write_json(model_list, path, auto_unbox=T, null="null", pretty=4)},
    error=function(e) {
      print_failure(glue::glue("Unable to write metadata to {path}: {e}"))
      stop(e)
    })
}


#' Function to make a getter with default
#' @noRd
#' @export
make_getter <- function(name, default_name=NULL) {
  getter <- function(self) {
    get_val <- S7::prop(self, ".getter")(name)
    if(is.null(get_val) && !is.null(default_name)) {
      parent <- S7::prop(self, default_name)
      if((!is.null(parent)) && (name %in% S7::prop_names(parent))){
        get_val <- S7::prop(parent, ".getter")(name)
      }
    }
    return(get_val)
  }
  return(getter)
}

#' Function to make a setter
#' @noRd
#' @export
make_setter <- function(name) {
  setter <- function(self, value) {
    S7::prop(self, ".setter")(name, value)
    return(self)
  }
  return(setter)
}


#' Function to help with caching of the metadata file, to avoid unecessary
#' reads and writes.
#' 
#' @param path The path to the metadata file.
cache_helper <- function(path, model) {
  # Stats for the file
  file_stats <- fs::file_info(path)
  cache_size <- file_stats$size
  cache_mtime <- file_stats$modification_time
  cache_model <- read_model(path, model)
  
  reload <- function(force=F){
    # Get current stats of the file
    cur_stats <- fs::file_info(path)
    cur_size <- cur_stats$size
    cur_mtime <- cur_stats$modification_time
    # Check against cache, if different we update the loaded value.
    if(force || ((cur_size != cache_size) || (cur_mtime != cache_mtime))){
      cache_model <<- read_model(path, model)
      cache_size <<- cur_size
      cache_mtime <<- cur_mtime
      print_info(glue::glue("{path} reloaded."))
    }
  }
  
  getter <- function(name){
    # Reload if needed
    reload()
    # Retrieve value from model
    return(S7::prop(cache_model, name))
  }
  
  setter <- function(name, value){
    # Reload if needed
    reload()
    # Create an argument list with one element
    arg_list <- setNames(list(value), name)
    # Check if new value and current value are the same
    if(!identical(S7::prop(cache_model, name), value)){
      # Set value on model
      new_model <- rlang::inject(S7::set_props(cache_model, !!!arg_list))
      # Write value to disk.
      write_model(path, new_model)
    }
  }
  
  return(list(getter=getter, setter=setter, model=cache_model))
}
