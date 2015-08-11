#' Simple Round
#'
#' \code{sround} returns the row-wise average of a series of vectors. The n argument
#' indicates the minimimum number of items a particpant should have responded
#' to in order to be given an average.
#'
#' @param x A numeric vector to be rounded
#' @param n Number of decimals to round to (default is to integer)
#' @return A vector of type numeric.
#' @examples
#' \dontrun{
#' x <- seq(from = 1, to = 5, by = .25)
#' round(x)
#' sround(x)
#' round(x, 1)
#' sround(x, 1)
#' }
#' @seealso \code{\link{scoring}}
#'

sround <- function(x, n = 0) {
  posneg <- sign(x)
  z <- abs(x) * (10 ^ n)
  z <- z + 0.5
  z <- trunc(z)
  z <- z / (10 ^ n)
  z <- z * posneg
  return(z)
}
