#' Pretty p-values
#'
#' This function helps print p-values in RMD output
#'
#' @param pvals a vector of numeric p-values
#' @param sig.limit A lower threshold below which to print "<sig.limit"
#' @param digits how many digits to print?
#' @param html uses the HTML symbol instead of normal < symbol
#'
#' @return A character vector of "pretty" p values
#' @export
#'
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {

  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }

  sapply(pvals, function(x, sig.limit) {
    if(is.na(x))
      return(x)
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
      return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
    return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

#' List tables with the same columns
#'
#' This function will print a group of tables together in a decently pretty way.
#' May need a bit of finagling.
#'
#' @param tabs A list of data frames or tibbles with the same number and names of columns
#' @param bo boostrap options to pass to kableExtra
#' @param ... other options to be passed to kable, such as caption, align, etc.
#'
#' @return HTML or Latex output for pretty tables
#' @export

list_kables <- function(tabs, bo = c("striped", "condensed"), ...) {
  library(kableExtra)

  idx <- sapply(tabs, nrow)

  tabs %>%
    bind_rows() %>%
    kable(...) %>%
    kable_styling(bo, full_width = FALSE) %>%
    group_rows(index = idx)
}
