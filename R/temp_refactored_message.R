# Functions to warn users of refactored methods and provide the renamed function.
# These are expected to be short term/temporary functions to aid in transition
# after refactoring to provide consistent naming across the package.
# Perhaps remove following deprecation of the methods marked for deprecation.


#'
#' @noMd
#' @noRd

CIDA_drive_path <- function(file = "") {
  warning( paste(c("CIDA_drive_path() renamed to cida_drive_path()")),immediate. = TRUE)
}
