## ---- message=FALSE, eval=FALSE------------------------------------------
#  library('devtools')
#  install_github("mattsigal/mmisc")

## ------------------------------------------------------------------------
library('mmisc')

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
set.seed(77)
dat <- data.frame(Gender=sample(c("Male", "Female"), size = 1500, replace = TRUE),
                  AgeGrp=sample(c("18-39", "40-49", "50+"), size = 1500, replace = TRUE),
                  Relationship=sample(c("Direct", "Manager", "Coworker", "Friend"), 
                                      size = 1500, replace = TRUE),
                  X=rnorm(n=1500, mean=0, sd=1),
                  Y=rnorm(n=1500, mean=0, sd=1),
                  Z=rnorm(n=1500, mean=0, sd=1))
str(dat)

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
strata = c("Gender", "AgeGrp", "Relationship")

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
head(stratified_norm(dat, strata, return.grid = TRUE), n = 14)

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
samples <- c(36,34,72,58,47,38,18,18,15,22,17,10,24,28,11,27,15,25,72,70,52,43,21,27)

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
head(stratified_norm(dat = dat, strata = strata,
                    observations = samples, return.grid = TRUE), n = 14)

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
subset.data <- stratified_norm(dat, strata, samples, full.data = FALSE)

full.data <- stratified_norm(dat, strata, samples, full.data = TRUE)

str(subset.data)

str(full.data)

## ---- strip.white=TRUE, collapse=TRUE------------------------------------
ftable(xtabs(~Gender + AgeGrp + Relationship, data = subset.data))

## ---- strip.white=TRUE, collapse=TRUE, warning=FALSE, message=FALSE------
full.data1 <- stratified_norm(dat, strata, samples, full.data = TRUE)
full.data2 <- stratified_norm(dat, strata, samples, full.data = TRUE)
identical(full.data1, full.data2)

set.seed(77)
full.data1 <- stratified_norm(dat, strata, samples, full.data = TRUE)
set.seed(77)
full.data2 <- stratified_norm(dat, strata, samples, full.data = TRUE)
identical(full.data1, full.data2)

