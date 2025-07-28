#' Internal Utility Functions

#' Check String Parameter Values for errors
#'
#'
#' @param value parameter value to check
#' @return the value to be used after checking it is a character value and not an array or returning the first value of the array.
#'
#' @noRd
#' @noMd
#'
check_string_param_value <- function(value="",parameter=""){

  if(!is.character(value)) stop(parameter,' must be a character string')
  if(length(value) > 1) {
    warning('Only First String is Used')
    value <- value[1]
  }
  return(value)
}



#' Find and return the beginning of the full path up to the start of the common path.
#'
#'
#' @param full_path The full path to search for the partial path in.
#' @param partial_path The partial path which may start any were in the full path.
#' @return The beginning of the full path proceeding the start of the partial path.
#'
#' @noRd
#' @noMd
#'
find_drive_location <- function(full_path="",partial_path=""){
  parts_full<- fs::path_split(full_path)[[1]]
  parts_partial <- fs::path_split(partial_path)[[1]]
  common_index_start=1
  if(parts_partial[common_index_start]=="/"){
    common_index_start=2
  }
  match_index <- -1
  for (i in seq_along(parts_full)){
    if(parts_full[i]==parts_partial[common_index_start]){
      oldI=i
      is_match=TRUE
      for (j in common_index_start:length(parts_partial)){
        if(parts_partial[j] != parts_full[i]){
          is_match=FALSE
          break
        }
        i <- i+1
      }
      if(is_match){
        match_index<-oldI-1
      }
      i=oldI
    }
  }

  drive_parts <- head(parts_full,match_index)
  drive_path <- fs::path_join(drive_parts)[[1]]
  return(drive_path)
}


#' Deprecation Warning for functions flagged to be deprecated
#' deprecation_warn() - print a warning that the function will be deprecated.
#'
#' @param function_name Function name the warning was called from.
#'
#' @noRd
#' @noMd
#'
deprecation_warn <- function(function_name=""){
  warning(paste(c(function_name,"() has been deprecated and will be removed in ",
                    "future package versions.\nPlease contact ",
                    "max.mcgrath@cuanschutz.edu if you regularly use this ",
                    "function\nand would like to see it stay.")),
          call.=FALSE,immediate. = TRUE)
}



#' Deprecated Warning for functions already deprecated
#'
#' deprecated_warn() - function to call for deprecated functions listing the
#' version that removed the function
#'
#' @param function_name Version of package function was removed.
#' @param version Version of package function was removed.
#'
#' @noRd
#' @noMd
#'
deprecated_warn <- function(function_name="", version=""){
  warning(paste(c(function_name,"() was deprecated in version ",version)),call.=FALSE,immediate. = TRUE)
}


