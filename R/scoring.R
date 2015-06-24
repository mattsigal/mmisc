#' Normative Scoring
#'
#' \code{scoring} is a dual purpose function that returns a full dataframe that has
#' either been converted into standard scores based upon overall norms, or by
#' age and gender specific norms (based upon the norm argument). Age and Gender
#' are included as arguments simply to designate the names of the respective variables.
#'
#' This function requires an external R dataframe called `scoring.table` that has the following structure: columns
#' called SCALE (with scale names that match those in `dat`), AGE, GENDER, MEAN, and SE.  For the rows pertaining to
#' overall norms, the AGE and GENDER values should be the character string "overall". See the example code for more details.
#'
#' @param dat An R dataframe object
#' @param norm A character vector of length 1. Use "overall" for overall norms, and "agegender" for age and gender specific norms.
#' @param Age A character vector of length 1, used to indicate the name of the Age variable in dat.
#' @param Gender A character vector of length 1, used to indicate the name of the Gender variable in dat.
#' @return An R dataframe object with N rows and K columns, where K is the number of scales included in dat.
#' @examples
#' \dontrun{
#' set.seed(77)
#' scoring.table <- data.frame(SCALE = rep(LETTERS[1:5], 5),
#'                             AGE = c(rep("overall", 5),
#'                                     rep(c("Young", "Old"), each = 5),
#'                                     rep(c("Young", "Old"), each = 5)),
#'                             GENDER = c(rep("overall", 5), rep(c("Male", "Female"), each = 10)),
#'                             MEAN = rnorm(25, mean = 100, sd = 10),
#'                             SD = rnorm(25, mean = 10, sd = 3),
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
#' overall <- scoring(dat, norm = "overall")
#' agegen <- scoring(dat, norm = "agegender")
#' }
#' @seealso \code{\link{scoring}}
#'

scoring <- function(dat, norm = "overall", Age = "AGE", Gender = "GENDER"){
    tmpDat <- dat[, -which(names(dat) %in% c(Age, Gender))]
    demos <- dat[, which(names(dat) %in% c(Age, Gender))]
    out <- tmpDat

    if (norm == "overall"){
      for (i in 1L:length(tmpDat)){
        scale <- names(tmpDat)[i]
        scalemean <- scoring.table[scoring.table$SCALE == scale & scoring.table$AGE == "overall", 4]
        scalesd <- scoring.table[scoring.table$SCALE == scale & scoring.table$AGE == "overall", 5]
        std.score <- 100 + 15 * ((tmpDat[,i] - scalemean) / scalesd)
        out[,i] <- std.score
      }
     return(out)
   } else if (norm == "agegender"){
      for (i in 1L:length(tmpDat)){
        scale <- names(tmpDat)[i]
        for (j in 1L:nrow(tmpDat)){
          scalemean <- scoring.table[scoring.table$SCALE == scale &
                                       scoring.table$AGE == demos[j, Age] &
                                       scoring.table$GENDER == demos[j, Gender], 4]
          scalesd <- scoring.table[scoring.table$SCALE == scale &
                                       scoring.table$AGE == demos[j, Age] &
                                       scoring.table$GENDER == demos[j, Gender], 5]
          std.score <- 100 + 15 * ((tmpDat[j,i] - scalemean) / scalesd)
          out[j,i] <- std.score
        }
      }
     return(out)
   } else {
     return("Norm argument should be either overall or agegender.")
   }
}

