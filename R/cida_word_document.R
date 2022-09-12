#' A wrapper fn for CIDA word doc output
#'
#' This is a wrapper for the rmarkdown::word_document function.
#'
#' @param ... passed to rmarkdown::word_document
#'
#' @export
cida_word_document <- function(...){

  # Define filepaths
  sty_file <- system.file("rmarkdown/", "CIDAStyles.docx", package="CIDAtools")

  # supply files to your custom format
  rmarkdown::word_document(..., reference_docx = sty_file)
}
