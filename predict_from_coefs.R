#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){

  y <- dat %>% pull({{response}})

  explanatory <- dat %>%
    select(-{{response}})

  x <- as.matrix(explanatory)

  x_with_ones <- cbind(1, x)

  x_with_ones

  predicted <- x_with_ones %*% t(as.matrix(coefs)) #reformatted from being transposed

  result <- data.frame(True = y, Predicted = predicted)

  return(result)

}
