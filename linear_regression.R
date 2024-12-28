#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here

  sd_x <- sd(x)
  sd_y <- sd(y)

  x_with_ones <- cbind(1, x) #works as intended

  x_crossprod_inverse <- solve(crossprod(x_with_ones))
  x_by_y <- t(x_with_ones) %*% y

  a <- x_crossprod_inverse %*% x_by_y

  beta_0 <- a[1]
  beta_1 <- a[2]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
multiple_linear_regression <- function(dat, response, method = NULL) {

  y <- dat %>% pull({{response}})

  explanatory <- dat %>%
    select(-{{response}})

  x <- as.matrix(explanatory)

  x_with_ones <- cbind(1, x)

  x_crossprod_inverse <- solve(crossprod(x_with_ones))
  x_by_y <- t(x_with_ones) %*% y

  a <- as.data.frame(t(x_crossprod_inverse %*% x_by_y)) #transposed for formatting purposes

  colnames(a)[1] <- "Intercept"

  results <- a ### This should be a data frame, with columns named
                ### "Intercept" and the same variable names as dat.

  return(results)

}
