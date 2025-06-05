#' Internal Utility Functions


#'
#' deprecation_warn() - print a warning that the function will be deprecated.
#'
#' @param function_name Function name the warning was called from.
#'
#' @noRd
#'
deprecation_warn <- function(function_name=""){
  warning(paste(c(function_name,"() has been deprecated and will be removed in ",
                    "future package versions.\nPlease contact ",
                    "max.mcgrath@cuanschutz.edu if you regularly use this ",
                    "function\nand would like to see it stay.")),
          call.=FALSE,immediate. = TRUE)
}



#'
#' deprecated_warn() - function to call for deprecated functions listing the
#' version that removed the function
#'
#' @param function_name Version of package function was removed.
#' @param version Version of package function was removed.
#'
#' @noRd
#'
deprecated_warn <- function(function_name="", version=""){
  warning(paste(c(function_name,"() was deprecated in version ",version)),call.=FALSE,immediate. = TRUE)
}
