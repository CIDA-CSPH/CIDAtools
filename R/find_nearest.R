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



