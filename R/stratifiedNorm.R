#' Function to draw observations from stratified norm sample
#'
#' This function creates a stratified random sample, given an arbitrary number of
#' factors and levels within those factors.
#'
#' @param dat An R data.frame object
#' @param strata character; a named vector indicating the strata variables
#' @param observations numeric; number of cases to sample from each strata
#' @param return.grid logical; return grid of factor combintions?
#' @param full.data logical; return the original dataset with a new logical
#'   variable (named from 'full.data.id') indicating whether the row was selected
#' @param full.data.id a string pertaining the variable name used when 'full.data=TRUE'
#' @return An R data.frame object, either pertaining to a grid of factor combinations or the stratified sample
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(Gender=sample(c("Male", "Female"), size = 1500, replace = TRUE),
#'                AgeGroup=sample(c("18-39", "40-49", "50+"), size = 1500, replace = TRUE),
#'                Relationship=sample(c("Direct", "Manager", "Coworker", "Friend"),
#'                     size = 1500, replace = TRUE),
#'                X=rnorm(n=1500, mean=0, sd=1),
#'                Y=rnorm(n=1500, mean=0, sd=1),
#'                Z=rnorm(n=1500, mean=0, sd=1))
#'
#' strata = c("Gender", "AgeGroup", "Relationship")
#'
#' stratifiedNorm(dat, strata, return.grid=TRUE)
#' test <- stratifiedNorm(dat, strata, counts = 5, return.grid=FALSE)
#' head(test)
#'
#' stratifiedNorm(dat, strata, counts = 5, full.data=TRUE)
#' }
#'
stratifiedNorm <- function(dat, strata, observations=0, return.grid=FALSE, full.data=FALSE, full.data.id = "sampled"){
  if (class(dat) != "data.frame")
    stop("Data input is not a dataframe.")
  if (!all(strata %in% names(dat)))
    stop("Please verify your 'strata' arguments exist in dataframe.")
  if (class(observations) != "numeric")
    stop("Please ensure the observations vector is numeric.")

  nstrata <- length(strata)
  svars <- dat[,strata]
  factors <- list()

  dat <- cbind(InTeRnAlIdVaRs=as.numeric(rownames(dat)), dat)

  for (i in 1:nstrata){
    if (class(svars[,i]) != "factor"){
      stop("Strata variable is not a factor.")
    }
    factors[[i]] <- levels(svars[,i])
  }

  names(factors) <- strata

  remove <- expand.grid(factors)
  if(length(observations) == 1)
    observations <- rep(observations, nrow(remove))

  if(length(observations) < nrow(remove))
    stop("Observations vector does not have enough elements.")

  if(length(observations) > nrow(remove))
    stop("Observations vector has too many elements.")

  if(return.grid) return(cbind(remove, Observations = observations))

  ret <- lapply(1:nrow(remove), function(ind, dat, remove, count){
    pick <- matrix(FALSE, nrow(dat), ncol(remove))
    names <- colnames(remove)
    for(i in 1L:ncol(pick))
      pick[,i] <- dat[,names[i]] == remove[ind,names[i]]
    pick <- rowSums(pick) == ncol(remove)
    tmpsvars <- dat[pick,]
    if(nrow(tmpsvars) > count[ind]){
      return(tmpsvars[sample(1:nrow(tmpsvars), count[ind]), ])
    }
    if(nrow(tmpsvars) == count[ind]){
      message(sprintf('Combination for (%s) is equal to count. Returning all observations.',
                      paste0(as.matrix(remove[ind,]), collapse = '|')))
      return(tmpsvars)
    } else {
      message(sprintf('Combination for (%s) has LESS than count. Returning all observations.',
              paste0(as.matrix(remove[ind,]), collapse = '|')))
      return(tmpsvars)
    }
  }, dat=dat, remove=remove, count=observations)

  newdat <- do.call(rbind, ret)
  IDINT <- dat$InTeRnAlIdVaRs %in% newdat$InTeRnAlIdVaRs
  dat$InTeRnAlIdVaRs <- newdat$InTeRnAlIdVaRs <- NULL

  if(full.data == FALSE){
    return(newdat)
  } else {
      if(full.data.id %in% colnames(dat)){
          warning('full.data.id name exists in original data. Placing id name in first column with
                  a unique name')
          dat <- cbind(IDINT, dat)
      } else {
        dat[[full.data.id]] <- IDINT
      }
    return(dat)
  }
}