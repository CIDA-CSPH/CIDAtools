#' Create a table one
#'
#'
#' This is a function created to provide characteristics of a study group with
#' an option to stratify by some variable (usually an exposure) The output of
#' this function is designed to be used with pander in rmarkdown, but all row
#' name formatting can be removed with the option: emphasis = 'n'.
#'
#' @param data the data frame or design object of the data you which to
#' characterize
#' @param rowvars A vector of row positions or names.
#' @param colvar The position or name of the variable to stratify by, set
#' to \code{NULL} for no stratification.(p values cannot be returned if
#' \code{NULL})
#' @param sigfig The number of significant digits to use for mean, sd, median,
#' and IQR.
#' @param rowvar_names An optional vector of row names to be used for
#' variables. Must be the same length and in the same order as rowvars.
#' @param incl_missing Set to \code{TRUE} to include missing values (default)
#' @param incl_pvalues Set to \code{TRUE} to include p values (p values are only
#' calculated on non missing observations) (default = \code{FALSE})
#' @param emphasis Set to \code{'s'} for to indent categories for categorical values,
#' \code{'b'} to bold just variable names, and \code{'n'} for no emphasis.
#' @param MedIQR optional vector of continuous variables to return median and
#' IQR instead of mean and SD.
#' @param asTable should a table (\code{TRUE}) or a matrix be returned
#' @param lineBreaks should the N be printed on a seperate line from the
#' categories. default is \code{TRUE} (see details)
#' @param tight if \code{TRUE} (default) no spaces between numbers and
#' parenthesis
#' @param verbose should both levels of binary variables be printed.
#' @param ... Arguments passed through methods to table 1.
#' @return  a table/matrix with N and percentages for categorical variables, mean
#' and sd for continuous ones, and median and 25th and 75th percentile for integers.
#' If variables are passed via the MedIQR argument,
#' median and 25th and 75th percentile is calculated instead for those variable.
#' @details  Only
#' the 1st factor of binary categorical variables is displayed. All
#' determinations of categorical,
#' binary, or continuous are performed automatically.
#' Character variables are converted to factors. Variables are displayed in
#' the following order: binary, non-binary categorical, continuous, and integers
#' + continuous variables with median and IQR.
#' If no stratification variable is provided, summary statistics on the
#' entire sample are provided. No p-values can be provided in this case.
#' If a design object is passed in lieu of a data frame, weighted numbers
#' using the survey package are provided.
#' (The survey package must be installed in this case.)
#'
#' To use the lineBreaks use \code{panderOptions('keep.line.breaks', TRUE)}
#' @keywords table1 tableone characteristic
#' @export
#'




Table1 <- function(data, ...){
  UseMethod('Table1')
}

#' @describeIn Table1 unweighted table 1
#' @export

Table1.data.frame <- function(data, rowvars, colvar, sigfig = 4,
                              rowvar_names = NULL, incl_missing = TRUE,
                              incl_pvalues = FALSE,
                              emphasis = c('b', 's', 'n'),
                              MedIQR = NULL, asTable = TRUE, lineBreaks = T,
                              tight = TRUE, verbose = FALSE, ...){
  message("The Table1() function is no longer being maintained and will be deprecated in a future release of CIDAtools. Please use cida_table1() instead.")
  # set the home calling environment
  thisisthehomecallingenvironment <- T

  # set emphasis
  emphasis <- match.arg(emphasis)

  # get dataframe for row variables
  nl <- as.list(seq_along(data))
  names(nl) <- names(data)
  rows <- data[, eval(substitute(rowvars), nl, parent.frame()), drop = F]

  # do not include p_values if data is not stratified
  # setup dummy variable for unstratified data
  if (is.null(eval(substitute(colvar), nl, parent.frame()))) {
    incl_pvalues <- F
    data$dummy <- factor(rep('', nrow(data)))
    colvar <- 'dummy'
    nl <- as.list(seq_along(data))
    names(nl) <- names(data)
    dummy <- T
  }

  # column stratificiation variable
  y <- data[, eval(substitute(colvar), nl, parent.frame())]


  # set rows with MedIQR requests
  median_rows <- which(names(rows) %in% MedIQR)
  for(i in median_rows){class(rows[, i]) <- c('MedIQR', 'numeric')}; remove(i)

  # get number of levels and sort so binary is on top only if no missing
  n_levs <- sapply(lapply(rows, function(x){
      if(is.character(x) | is.logical(x)){
        y <- levels(factor(x))} else y <-levels(x)
      }), length)
  if(incl_missing == T) {
    add_miss <- sapply(rows[!is.na(y), ], function(x) any(is.na(x)))
    n_levs <- n_levs + add_miss
    }
  if(!verbose) n_levs[n_levs != 2] <- 3

  # sort rows by class, want MedIQR last...
  cls <- sapply(lapply(rows, class), `[[`, 1)
  cls[cls %in% c('character', 'logical')] <- 'factor'
  cls[cls %in% c('integer')] <- 'zzz'
  cls[cls == 'MedIQR'] <- 'zzz'

  # ord won't work if there's only one row var
  ord <- 1
  if(length(rows) != 1) ord <- order(cls, n_levs)

  # set names if present
  if(!is.null(rowvar_names)) names(rows) <- rowvar_names

  # order rows
  rows <- rows[, ord, drop = F]

  # add bold if requested
  if(emphasis == 'b') names(rows) <- paste0('**', names(rows), '**')


  ##################################################################
  ### headers
  # number of columns
  Cols <- length(levels(y))
  p_col <- NULL

  # add p_value if required
  if(incl_pvalues) p_col <- ''

  # set section headers (NULL if not needed)
  N_pct <- c('', rep('N(%)', Cols), p_col)
  if(sum(cls == 'factor') == 0) N_pct <- NULL
  Mean_sd <- c('', rep('Mean(SD)', Cols), p_col)
  if(sum(cls == 'numeric') == 0) Mean_sd <- NULL
  Median <- c('', rep('Median(IQR)', Cols), p_col)
  if(sum(cls == 'zzz') == 0) Median <- NULL

  # get table
  tbl <- lapply(rows, returnRow, y = y, p = incl_pvalues)
  if(!tight) tbl <- lapply(tbl, gsub, pattern = '\\(', replacement = ' \\(')

  # set class vector to same order as row
  cls <- cls[ord]
  # bind rows by type
  cats <- do.call(rbind, tbl[cls == 'factor'])
  means <- do.call(rbind, tbl[cls == 'numeric'])
  medians <- do.call(rbind, tbl[cls == 'zzz'])

  # put it all together
  tbl <- rbind(N_pct, cats, Mean_sd, means, Median, medians)

  # add p value label
  if(incl_pvalues) p_col <- 'P value'

  # Make column headers
  Nequals <- ' \\\n N = '
  if(!lineBreaks) Nequals <- ' N = '
  if (exists('dummy', inherits = F)) Nequals <- 'N = '

  Stratified_N <- table(y)
  Stratified_N <- format(Stratified_N, big.mark = ',', trim = T)
  Stratified_N <- paste0(levels(y), Nequals, Stratified_N)
  Header <- c('', Stratified_N, p_col)
  tbl <- rbind(Header, tbl)
  if(asTable) tbl <- as.table(tbl)
  rownames(tbl) <- NULL
  colnames(tbl) <- NULL
  return(tbl)
}

