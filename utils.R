type_check <- function(variable, type){
  if(typeof(variable) != type){
    error <- paste0(variable, " should be of type ", type, " is of type ", typeof(variable), ".")
    stop(error)
  }
}

## Modified R code for fitting learning curve
##
## Source:
## Figueroa et al. 2012 BMC Medical Informatics and Decision Making
##
## Modifications:
## Use notation from Mukherjee et al. 2003 JCB
## Reduced to nonlinear fitting
##
## Silke Szymczak, IMBS
## January 2022

## August 2022
## modifications:
## - replace d by alpha
## - set start values

#' @title Fit learning curve
#'
#' @param x [numeric(n)] vector of sample sizes
#' @param y [numeric(n)] vector of classification errors
#'
#' @return Fitted model of class nls
#'
#' @references Source: Figueroa et al. 2012 BMC Medical Informatics and Decision Making
fit_learning_curve <- function(x, y) {

  m = nls(y ~ b + a * x^(-alpha),
          ## SSzy
          ## add start parameters
          start = list(a = 1, b = 0, alpha = 0.5),
          control = list(maxiter = 1000, warnOnly = TRUE),
          upper = list(a = 10, b = 10, alpha = 10),
          lower = list(a = 0, b = 0, alpha = 0.1),
          algorithm = "port",
          data = data.frame(y = y, x = x))

  return(m)
}

library(mlr3learners)
