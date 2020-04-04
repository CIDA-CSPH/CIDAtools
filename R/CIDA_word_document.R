#' Custom CIDA Word template
#'
#' Loads additional template file
#'
#' @param ... additional arguments provided to \@code{word_document}
#' @export
#'
CIDA_word_document = function(...) {

  # find the word doc template w/in CIDAtools
  file <- system.file('rmarkdown/templates/report_word', 'word_style_reference.docx', package = 'CIDAtools')

  # call the base word__document function
  rmarkdown::word_document(reference_docx = file, ...)
}
