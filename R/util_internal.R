#' Internal Utility Functions

#' Check String Parameter Values for errors
#'
#'
#' @param value parameter value to check
#' @return bool if it passes the check otherwise stops on error.
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
  return(TRUE)
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


