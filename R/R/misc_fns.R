

#' Get pretty numbers of rows - To be deprecated
#'
#'
#' Retrieve the number of rows in dataframe of matrix with commas inserted for
#' nice reports.
#'
#' @param x data frame or matrix
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynrow
#' @export
#'



nrowP <- function(x){
  deprecation_warn("misc_fns.nrowP")
  format(nrow(x), big.mark = ',', trim = T)
}

#' Get pretty number of levels - To be deprecated
#'
#'
#' Just a wrapper for format(nlevels) with big.mark = , and trim = T
#'
#' @param x factor
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynlevels
#' @export
#'

nLevelsP <- function(x){
  deprecation_warn("misc_fns.nLevelsP")
  format(nlevels(x), big.mark = ',', trim = T)
}



#' Convert Interval Notation - To be deprecated
#'
#' Converts a vector from Interval Notation to less than equal to, less than,
#' etc.
#'
#' @param x a character vector to be converted
#'
#' @return a character vector of same length of x converted
#' @keywords interval notation
#' @export
#'
convertIntervalNotation <- function(x){
  deprecation_warn("misc_fns.convertIntervalNotation")
  if(!is.character(x)) stop('x must be a character vector')
  x <- gsub('\\(-Inf, ', '', x)
  x <- gsub(',Inf\\)', '', x)
  x <- gsub('\\[', '\u2265', x)
  x <- gsub('([0-9]+)\\]', '\u2264\\1', x)
  x <- gsub(',', ' - ', x)
  x <- gsub("\\(", '>', x)
  x <- gsub("([0-9]+)\\)", "<\\1", x)
  return(x)
}

#' Round and don't drop trailing zeros - To be deprecated
#'
#' Shorter wrapper for format(x, digits = n, nsmall = n)
#'
#' @param x numeric to be formatted
#' @param n number of digits for nsmall
#'
#' @return a character vector of same length of x converted
#' @details should not be used unless digits after a decimal are needed.
#' Note for numbers with leading zeros (ie. 0.0349) you will get one more
#' decimal place than n. (ie. \code{Round(O.0349, 2)} will return
#' \code{0.035})
#'
#' @keywords interval notation
#' @export
#'
#'
Round <- function(x, n){
  deprecation_warn("misc_fns.Round")
  format(x, digits = n, nsmall = n)
}

#' Sum ignoring NAs - To be deprecated
#'
#' Will sum values returning NA only if all values are NA, otherise will ignore
#'
#' @param ... numbers or vectors to be summed. Must be type logical or numeric.
#'
#' @return a numeric vector of the same length as the arguments
#' @details this function will provide vectorized sums with NAs ignored unless
#' only NAs are present
#'
#' @keywords sum
#' @export
#' @examples
#' # ignores NA
#' sum_ignore_NA(2, 3, NA)
#' # returns NA if all values are NA
#' sum_ignore_NA(NA, NA, NA)
#'
#' # returns vectorized sums
#'
#' x <- c(1, 2, NA)
#' y <- c(1:3)
#' sum_xy <- sum_ignore_NA(x, y)
#' data.frame(x, y, sum_xy)
#'
#' x <- c(1, 2, NA)
#' y <- c(1, 2, NA)
#' sum_xy <- sum_ignore_NA(x, y)
#' data.frame(x, y, sum_xy)


sum_ignore_NA <- function(...){
  deprecation_warn("misc_fns.sum_ignore_NA")
  arguments <- list(...)
  arguments <- lapply(arguments, unlist)
  x <- sapply(arguments, length)
  if(min(x) != max(x)) stop('Vectors must be same length')
  arguments <- lapply(1:min(x), function(i) sapply(arguments, `[[`, i))
  sapply(arguments, function(numbers){
    if(all(is.na(numbers))) return(NA)
    if(!is.numeric(numbers) & !is.logical(numbers))
      stop('Arguments must be numeric or logical')
    sum(numbers, na.rm = T)
  })
}

#' Vectorized power estimates - To be deprecated
#'
#'
#' This function allows you to use power.t.test, power.prop.test, etc in
#' vectorized fashion and return a table of results
#'
#' @param fun a power calculating function; <function>
#' @param ... the arguments for the power calculating function assigned to `fun`;
#' @return tibble of results
#'
#' @importFrom generics tidy
#' @importFrom stats na.omit
#'
#' @examples
#' # single non-vectorized output
#' vec_power(fun = power.t.test, n = 100, delta = 1, sd = 1, sig.level = 0.05)
#'
#' # multiple vectorized output
#' vec_power(fun = power.t.test, n = 80:100, delta = 1, sd = 1, sig.level = 0.05)
#'
#' # every combination of arguments vectorized output
#' vec_power(fun = power.t.test, n = 90:100, delta = 1, sd = seq(0, 1, length=10), sig.level = 0.05)
#'
#' @export
#'

vec_power <- function(fun = stats::power.t.test, ...){
  deprecation_warn("misc_fns.vec_power")
  args <- list(...)
  params <- expand.grid(args, stringsAsFactors = FALSE)[,length(args):1]

  results <- tidy(do.call(fun, params[1,]))
  for(i in 1:nrow(params)) {
    res <- try(do.call(fun, params[i,]), silent = TRUE)
    results[i,] <- NA
    if(class(res)[1] != "try-error")
      results[i,] <- tidy(res)
  }

  results <- dplyr::bind_cols(results, params[!(names(params) %in% names(results))])

  return(na.omit(results))
}

#' Helper for pwr package version of power fns. - To be deprecated
#' @param x description
#' @param ... description
#'
tidy.power.htest <- function(x, ...) {
  deprecation_warn("misc_fns.tidy.power.htest")
  class(x) <- "list"
  as.data.frame(x)
}

#' Find the nearest observation to another observation  - To be deprecated
#'
#'
#' This function finds the nearest y to every x. Y's may be duplicated.
#'
#' @param x a vector to find matches for
#' @param y a vector to find the matches
#' @param direction default = both, ascending for only y matches before x and
#' descending for only y matches after x.
#' @param returnIndex should an index of the mathced y values be returned
#' instead of the matched list.
#' @return  a list of length 2 with a y matched to every x, note if direction =
#' 'ascending' or 'descending', NAs will be returned for x values with no y
#' values before or after, respectively. OR an index of matched y values if
#' \code{returnIndex = TRUE}.
#' @keywords findnearest
#' @references This function borrowed heavily from this stack exchange post:
#' https://stats.stackexchange.com/questions/161379/quickly-finding-nearest-time-observation
#' @export
#'
find_nearest <- function(x, y,
                         direction = c('both', 'ascending', 'descending'),
                         returnIndex = FALSE) {
  
  deprecation_warn("find_nearest.find_nearest")
  
  direction <- match.arg(direction)
  a <- switch(direction, both = T, ascending = T, descending = F)
  d <- switch(direction, both = T, ascending = F, descending = T)
  i <- order(order(x))
  if(returnIndex) j <- order(order(y))
  x <- x[order(x)]
  y <- y[order(y)]
  if(a) i_lower <- getlower(x, y)
  
  if(direction == 'ascending') {
    i_lower[y[i_lower] > x] <- NA
    if(returnIndex) return(match(i_lower, j)[i])
    return(list(x[i], y[i_lower][i]))
  }
  if(d) {
    i_upper <- getlower(rev(x), rev(y), upper = T)
    i_upper <- rev(rev(seq_along(y))[i_upper])
  }
  if(direction == 'descending') {
    i_upper[y[i_upper] < x] <- NA
    if(returnIndex) return(match(i_upper, j)[i])
    return(list(x[i], y[i_upper][i]))
  }
  lower_nearest <- x - y[i_lower] < y[i_upper] - x
  lower_nearest[is.na(i_upper)] <- T
  lower_nearest[is.na(i_lower)] <- F
  y_i <- i_lower
  y_i[!lower_nearest] <- i_upper[!lower_nearest]
  if(returnIndex) {
    y_i <- match(y_i, j)
    return(y_i[i])
  }
  y <- y[y_i]
  return(list(x[i],y[i]))
}



#' Internal function for find_nearest - To be deprecated
#'
#'
#' @param x first value
#' @param y second value
#' @param upper upper value?
#' @return  indexes of y for each x
#' @describeIn find_nearest function for finding lower(upper) value

getlower <- function(x, y, upper = FALSE){
  
  deprecation_warn("find_nearest.getlower")
  
  n <- length(y)
  z <- c(y, x)
  j <- i <- order(z, decreasing = upper)
  j[j > n] <- -1
  x_max <- cummax(j)
  x_max[x_max == -1] <- 1
  return (x_max[i > n])
}

#' Randomizer and blinder tool - To be deprecated
#'
#' See the inst/ folder for the main code for this function
#'
#' @export
randblinder_shiny_tool <- function() {
  .Defunct("shiny_tool", msg = "This function has been moved to the 'randblinder' package (https://github.com/CIDA-CSPH/randblinder). After installing, run randblinder::shiny_tool().")
}

#' Read in xlsx with fill colour - To be deprecated
#'
#' Reads in the fill colour of excel workbooks. Creates a data frame for each
#' sheet in a list if mutliple sheets are requested. Creates a colour column for
#' each colour column specified.
#'
#' @param file the path to the file you intend to read. Can be an xls or xlsx format.
#' @param colorColumns column numbers for which you want to read the colour,
#' for multiple sheets pass a list for each sheet (see details). For no colour
#' columns pass zero.
#' @param sheet NULL(default) for all sheets otherwise a vector of sheet numbers
#' or names to read.
#' @param header should the 1st row be read in as a header? defaults to T.
#' @details For \code{colourColumns} pass a list of numeric vectors for each
#' sheet. For example for 2 sheets \code{colourColumns = list(c(1,2), c(3))} for
#' columns 1 and 2 in the first sheet and 3 in the second. If the list of
#' \code{colourColumns}
#' is shorter than the sheets the remaining sheets will be assumed to have no
#' colour columns. If the list of colour columns is longer only the first n
#' elements will be used where n is the number of sheets with a warning.
#' @return A data frame for one sheet or a list of data frames for multiple sheets
#' @export
#' @keywords Excel colour color xlsx
#'
read_xlsx_color <- function(file, colorColumns, sheet = NULL, header = T){
  
  deprecation_warn("read_xlsx_color.read_xlsx_color")
  
  if(!requireNamespace("xlsx", quietly = TRUE))
    stop("package 'xlsx' is required.")
  if(!is.list(colorColumns) & is.numeric(colorColumns))
    colorColumns <- list(colorColumns)
  if(!all(sapply(colorColumns, is.numeric)))
    stop('Please pass coloured column by number')
  if(!is.null(sheet)){
    if(!is.numeric(sheet)&!is.character(sheet))
      stop('Sheets must be numbers or names')
  }
  wb <- xlsx::loadWorkbook(file)
  sheets <- xlsx::getSheets(wb)
  if(!is.null(sheet)) sheets <- sheets[sheet]
  if(length(colorColumns) < length(sheets)){
    add <- length(sheets) - length(colorColumns)
    colorColumns <- c(colorColumns, rep(list(0), add))
  }
  if(length(colorColumns) > length(sheets)){
    colorColumns <- colorColumns[seq_along(sheets)]
    warning(paste('Only the first', length(sheets),
                  'elements of colourColumns have been used.'))
  }
  createData <- function(sheet, colorColumns){
    rows <- xlsx::getRows(sheet)
    if(header) h <- -1 else h <- seq_along(rows)
    head <- xlsx::getCells(rows[-h])
    cells <- xlsx::getCells(rows[h])
    df <- data.frame(matrix(sapply(cells, xlsx::getCellValue),
                            nrow = length(rows) - header,
                            byrow = T))
    if(!is.null(head)) names(df) <- sapply(head, xlsx::getCellValue)
    
    cellColor <- function(style) {
      fg  <- style$getFillForegroundXSSFColor()
      rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
      rgb <- paste(rgb, collapse = "")
      return(rgb)
    }
    if(colorColumns[1] == 0) return(df)
    getColor <- function(col){
      styles <- sapply(xlsx::getCells(rows[h], col), xlsx::getCellStyle)
      colours <- sapply(styles, cellColor)
    }
    colours <- lapply(colorColumns, getColor)
    names(colours) <- paste('colour', seq_along(colours))
    df <- cbind(df, colours)
    return(df)
  }
  z <- mapply(createData, sheets, colorColumns, SIMPLIFY = F)
  if(length(sheets) == 1) return(z[[1]])
  return(z)
}

#' Pretty p-values - To be deprecated
#'
#' This function helps print p-values in RMD output
#'
#' @param pvals a vector of numeric p-values
#' @param sig.limit A lower threshold below which to print "<sig.limit"
#' @param digits how many digits to print?
#' @param html uses the HTML symbol instead of normal < symbol
#' @param equal_sign character value to append to front of p-value
#'
#' @return A character vector of "pretty" p values
#' @export
#'
pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE, equal_sign = "") {
  deprecation_warn("reporting_fns.pvalr")
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    paste0(equal_sign, res)
  }
  
  sapply(pvals, function(x, sig.limit) {
    if(is.na(x))
      return(x)
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else if (x <.01)
        return(roundr(x, digits = 3)) else
          return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

#' List tables with the same columns - To be deprecated
#'
#' This function will print a group of tables together in a decently pretty way.
#' May need a bit of finagling.
#'
#' @param tabs A list of data frames or tibbles with the same number and names of columns
#' @param bo boostrap options to pass to kableExtra
#' @param ... other options to be passed to kable, such as caption, align, etc.
#'
#' @return HTML or Latex output for pretty tables
#' @importFrom dplyr `%>%`
#' @export

list_kables <- function(tabs, bo = c("striped", "condensed"), ...) {
  deprecation_warn("reporting_fns.list_kables")
  idx <- sapply(tabs, nrow)
  
  tabs %>%
    dplyr::bind_rows() %>%
    knitr::kable(...) %>%
    kableExtra::kable_styling(bo, full_width = FALSE) %>%
    kableExtra::group_rows(index = idx)
}
