#' Flag Variables for Mismatches
#'
#' \code{flag_vars} is a utility function for comparing two vectors for equality. Messages indicating locations of
#' mismatches are printed to the console.
#'
#' @param x A vector of any type
#' @param y A vector of any type
#' @return boolean True or False indicating if all elements of x are equivalent to y.
#' #' @examples
#' \dontrun{
#' flag_vars(x = 1:10, y = 1:10)
#' set.seed(77)
#' dat <- data.frame(V1 = sample(1:2, 10, replace = TRUE), V2 = sample(1:2, 10, replace = TRUE))
#' dat
#' flag_vars(x = dat$V1, y = dat$V2)
#' }
flag_vars <- function(x, y){
  var1 <- deparse(substitute(x))
  var2 <- deparse(substitute(y))
  if (is.null(x)) return(paste0(var1, " does not exist in dataset."))
  if (is.null(y)) return(paste0(var2, " does not exist in dataset."))
  if (!isTRUE(all.equal(x, y))) {
    message(paste0(var1, " is NOT EQUAL to ", var2))
    message(paste("Mismatches were found on cases:", paste(which(x != y), sep = " ", collapse = ", ")))
    return(FALSE)
  } else {
    return(TRUE)
  }
}