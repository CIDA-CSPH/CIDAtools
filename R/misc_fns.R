

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
#'
tidy.power.htest <- function(x, ...) {
  deprecation_warn("misc_fns.tidy.power.htest")
  class(x) <- "list"
  as.data.frame(x)
}

#' helper function to cleanup project location
#' @export
proj_location_handler <- function(loc="") {

  loc <- gsub("/Volumes/sph-cida", "", loc)
  loc <- gsub("P:/", "", loc)
  loc <- gsub(".*BRANCHES", "BRANCHES", loc)
  loc <- gsub("/$", "", loc)
  return(loc)
}
