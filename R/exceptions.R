#' Apply Exceptions to a Dataframe
#'
#' \code{exceptions} is a dual purpose function that returns a full dataframe that has
#' either applied exceptions based upon an overall normative sample or exceptions
#' based upon age and gender specific norms (based upon the norm argument). Since
#' exceptions are applied based upon raw score values, the function requires both
#' dataframes. Exceptions are also passed to the function as a dataframe. Age and
#' Gender are included as arguments simply to designate the names of the
#' respective variables.
#'
#' This function requires an external R dataframe to be passed to exceptions.list
#' that has the following structure: columns called SCALE (with scale names that
#' match those in `dat`), AGE, GENDER, RAW, and SS.  Raw values pertain to problematic
#' values in terms of raw score, SS contains the values that should replace them
#' in the standard score rubric. For the rows pertaining to overall norms,
#' the AGE and GENDER values should be the character string "overall".
#' See the example code for more details.
#'
#' @param dat An R dataframe object pertaining to the raw data.
#' @param normedDat  An R dataframe object pertaining to the normed dataset.
#' @param exceptions.list An R dataframe object pertaining to the list of exceptions to be applied.
#' @param norm A character scalar, either "overall" or "agegender", depending if scoring should be done based upon overall values or age/gender specific norms.
#' @param Age A character vector of length 1, used to indicate the name of the Age variable in dat.
#' @param Gender A character vector of length 1, used to indicate the name of the Gender variable in dat.
#' @param maxVal A scalar indicating the value at which to "cap" scales. All values greater than maxVal are set to maxVal.
#' @return An R dataframe object with N rows and K columns, where K is the number of scales included in dat.
#' @examples
#' \dontrun{
#' set.seed(77)
#' exceptions.list <- data.frame(SCALE = rep(LETTERS[1:5], 5),
#'                               AGE = c(rep("overall", 5),
#'                                     rep(c("Young", "Old"), each = 5),
#'                                     rep(c("Young", "Old"), each = 5)),
#'                               GENDER = c(rep("overall", 5), rep(c("Male", "Female"), each = 10)),
#'                               RAW = rnorm(25, mean = 100, sd = 10),
#'                               SS = rnorm(25, mean = 10, sd = 3),
#'                             stringsAsFactors = FALSE)
#' set.seed(78)
#' n <- 500
#' dat <- data.frame(AGE = sample(c("Young","Old"), size = n, replace = TRUE),
#'                   GENDER = sample(c("Male","Female"), size = n, replace = TRUE),
#'                   A = rnorm(n, mean = 100, sd = 10),
#'                   B = rnorm(n, mean = 100, sd = 10),
#'                   C = rnorm(n, mean = 100, sd = 10),
#'                   D = rnorm(n, mean = 100, sd = 10),
#'                   E = rnorm(n, mean = 100, sd = 10))
#'
#' overall <- exceptions(dat[,3:7], overall, exceptions.list[1:5,], norm = "overall")
#' agegen <- exceptions(dat[,3:7], agegen, exceptions.list[6:25,], norm = "agegender",
#'                   Age = dat[,1], Gender = dat[,2])
#' }
#' @seealso \code{\link{scoring}}
#'

exceptions <- function(dat, normedDat, exceptions.list = NULL, norm = "overall", Age = NULL, Gender = NULL, maxVal = 132){
  if (is.null(exceptions.list)) return("Please supply the list of exception rules to exception.list.")

  out <- normedDat

  if (norm == "overall") {
    exceptions <- subset(exceptions.list, AGE == "overall" & GENDER == "overall")
    for (i in 1L:nrow(exceptions)) {
      out[,exceptions$SCALE[i]] <- ifelse(dat[,exceptions$SCALE[i]] == exceptions$RAW[i], exceptions$SS[i], out[,exceptions$SCALE[i]])
    }
  } else if (norm == "agegender") {
    Age <- dat[, Age]
    Gender <- dat[, Gender]
    dat <- round(dat[, names(normedDat)])
    exceptions <- subset(exceptions.list, AGE != "overall" & GENDER != "overall")
    for (i in 1L:nrow(exceptions)) {
      for (j in 1L:nrow(dat)) {
        if (is.na(Age[j])) next
        if (is.na(Gender[j])) next
        if (is.na(dat[j, exceptions$SCALE[i]])) next
        if (exceptions$AGE[i] == Age[j] & exceptions$GENDER[i] == Gender[j] & dat[j, exceptions$SCALE[i]] == exceptions$RAW[i]) {
          out[j, exceptions$SCALE[i]] <- exceptions$SS[i]
        }
      }
    }
    } else return("Set norm argument to either overall or agegender.")
    out[out > maxVal] <- maxVal
    out[out <= 0] <- 0
    return(out)
}