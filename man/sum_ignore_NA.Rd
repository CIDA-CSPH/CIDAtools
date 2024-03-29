% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_fns.R
\name{sum_ignore_NA}
\alias{sum_ignore_NA}
\title{Sum ignoring NAs}
\usage{
sum_ignore_NA(...)
}
\arguments{
\item{...}{numbers or vectors to be summed. Must be type logical or numeric.}
}
\value{
a numeric vector of the same length as the arguments
}
\description{
Will sum values returning NA only if all values are NA, otherise will ignore
}
\details{
this function will provide vectorized sums with NAs ignored unless
only NAs are present
}
\examples{
# ignores NA
sum_ignore_NA(2, 3, NA)
# returns NA if all values are NA
sum_ignore_NA(NA, NA, NA)

# returns vectorized sums

x <- c(1, 2, NA)
y <- c(1:3)
sum_xy <- sum_ignore_NA(x, y)
data.frame(x, y, sum_xy)

x <- c(1, 2, NA)
y <- c(1, 2, NA)
sum_xy <- sum_ignore_NA(x, y)
data.frame(x, y, sum_xy)
}
\keyword{sum}
