library(roxygen2)

#' Parse the cidatools Roxygen tag.
#'
#' This function is responsible for parsing the '@@cidatools' symbol
#' used to document public CIDAtools functions. We expect that the cidatools tag 
#' should be followed by two words (i.e. '@@cidatools <tag> <category>'). 
#' The first word denotes the tag, or name of the function as it appears in the 
#' documentation. The second word is the category of the function. The tag and
#' category allow us to link and categorize each R CIDAtools function with the 
#' equivalent implementations from the Python and CLI versions of CIDAtools.
#' 
#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_cidatools <- function(x) {
  return(tag_words(x, min=2, max=2))
}

#' 
#' @exportS3Method roxygen2::roxy_tag_rd
roxy_tag_rd.roxy_tag_cidatools <- function(x, base_path, env) {
  return(rd_section("cidatools", x$val))
}

#' 
#' @export
format.rd_section_cidatools <- function(x, ...) {
  return(paste0("\\cidatools{", x$value[[1]], "}", "{", x$value[[2]], "}"))
}