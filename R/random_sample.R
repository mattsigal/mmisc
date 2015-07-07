#' Creating Exploratory and Confirmatory Samples with Stratified Random Sampling
#'
#' \code{random_sample} returns a dataframe.
#'
#' This function is a slightly modified version of Ananda Mahto's `stratified` gist (https://gist.github.com/mrdwab/6424112).
#' Primary use is to generate exploratory and confirmatory samples, with the size = .5, and bothsets = TRUE.
#'
#' @param df An R dataframe object
#' @param group A character vector of the column or columns that make up the "strata"
#' @param size The desired sample size. If size is a value less than 1, a proporortionate sample is taken from each stratum. If
#' size is a single integer of 1 or more, that number of samples is taken from each stratum. If size is a vector of integers, the
#' specified number of samples is taken for each stratum. In this case it is recommended that you use a named vector, e.g.
#' `size = c(A = 5, B = 10)` if you wanted 5 samples from `A` and 10 from `B`
#' @param select This allows you to subset the groups in the sampling process. This is a list. For instance, if your group
#' variable was "Group", with values, 'A', 'B', and 'C', you could use `select = list(Group = c("A", C"))` to only sample from
#' `A` and `C`.
#' @param replace Should sampling be done with replacement?
#' @param bothSets If `size` is a scalar with a value less than one, should both sets be returned? Use `bothSets = TRUE` to
#' return both exploratory and confirmatory samples
#' @return If bothSets is TRUE, a list of length two, with each entry being an R dataframe object; otherwise, an R
#' dataframe object.
#' @examples
#' \dontrun{
#'  data(Cowles, package = "car")
#'  dat <- random_sample(Cowles, group = c("sex", "volunteer"), size = .5, bothSets = TRUE)
#' }
#' @seealso \code{\link{scoring}}, \code{\link{stratified_norm}}
#'

random_sample <- function(df, group, size, select = NULL, replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)

  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}