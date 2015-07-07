#' SPSS Style Means
#'
#' \code{mean_n} returns the row-wise average of a series of vectors. The n argument
#' indicates the minimimum number of items a particpant should have responded
#' to in order to be given an average.
#'
#' @param df An R dataframe object
#' @param n Minimum number of items with valid responses
#' @return A vector of type numeric.
#' @examples
#' \dontrun{
#' set.seed(77)
#' x <- data.frame(matrix(rnorm(n = 50), ncol = 5))
#' x[1,1:4] <- NA
#' mean_n(x, n = 2)
#' }
#' @seealso \code{\link{scoring}}
#'

mean_n <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ret <- ifelse(nvalid >= n, means, NA)
  return(ret)
}