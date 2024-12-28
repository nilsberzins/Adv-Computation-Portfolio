#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export

ridge_regression <- function(dat, response, lambda) {

  # changes dataframe to matrix
  dat_numeric <- as.matrix(dat)

  #extracts the response/explanatory variables from our dataset
  y <- dat_numeric[, response]
  x <- dat_numeric[, !colnames(dat_numeric) %in% response]

  #coefficient matrix
  lambda_diag <- diag(lambda, ncol(x))
  #x_transpose <- t(x)
  coef_matrix <- solve( t(x) %*% x + lambda_diag ) %*% t(x) %*% y

  #creates our dataframe
  results <- data.frame("Intercept" = coef_matrix[1], as.list(coef_matrix[-1]), "lambda" = lambda)

  return(results)

  #Refrenced Chat GPT
}



#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambdas A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {

  # creates dataframe
  lambda_errors <- data.frame(lambda = numeric(),
                              error = numeric(),
                              stringsAsFactors = FALSE)

  for (lambda in lambdas) {
    # coefficients called ridge_regression function
    coefficients <- ridge_regression(train_dat, response, lambda)

    # extracts the response/explanatory variables from our dataset
    y <- test_dat[, response]
    x <- test_dat[, !colnames(test_dat) %in% response]

    # predicted values
    y_predicted <- as.matrix(x) %*% t(as.matrix(coefficients[-1])) + coefficients[1]

    # sum of squared errors
    squared_error <- sum((y_predicted - y)^2)

    # binds variable to our dataframe
    lambda_errors <- rbind(lambda_errors, data.frame(lambda = lambda,
                                                     squared_error = squared_error))
  }

  return(lambda_errors)

  #Refrenced Chat GPT
}


