# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Building a Model with Top ten Features
#'
#' This function develops a prediction algorithm based on the top 10 features
#' in 'x' that are most predictive of 'y'
#'
#' @param x a n X p matric of n observations and p predictors
#' @param y a vector of length n representing the response
#' @return a vector of coefficients from the final fitted moel with top 10 features
#' @author Daniel Emaasit
#' @details
#' This function runs a univariate regression
#' @seealso \code{lm}
#' @export
#' @importFrom stats lm

topten <- function(x, y){
  p <- ncol(x)
  if (p < 10)
    stop("ther are less than 10 predictors")
  pvalues <- numeric(p)
  for( i in seq_len(p)) {
    fit <- lm(y ~x[, i])
    summ <- summary(fit)
    pvalues[i] <- summ$coefficients[2,4]
  }

  ord <- order(pvalues)
  ord <- ord[1:10]
  x10 <- x[, ord]
  fit <- lm(y ~ x10)
  coef(fit)
  }

#' Prediction with Top Ten Features
#'
#' @details
#' This function takes a set of coefficients produced by the \code{topten}
#' function and makes a prediction for each ot the values
#' @param X
#' @param b a vector of coefficeints obtained from the \code{topten} function
#' @return a numeric vector contatin the predictec values
#' @export

predict10 <- function(X, b) {
  X <- cbind(1, X)
  drop(X %*% b)
}
