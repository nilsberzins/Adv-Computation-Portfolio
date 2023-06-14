#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export

slr_gd <- function(dat, response, explanatory){

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  # Initializes intercept/slope/learning rate/max_interations variables
  start <- chooseStart(x, y)
  intercept <- start[1]
  slope <- start[2]
  learning_rate <- 0.01
  max_interations <- 1000

  # Gradient descent iterations
  for (iteration in 1:max_interations) {

    # predictions
    y_pred <- intercept + slope * x

    # gradients
    gradient_intercept <- (1 / length(y)) * sum(y_pred - y) #this is the partial derivative portion of the calculation
    gradient_slope <- (1 / length(y)) * sum((y_pred - y) * x) #this is the partial derivative portion of the calculation

    # Update
    intercept <- intercept - learning_rate * gradient_intercept
    slope <- slope - learning_rate * gradient_slope
  }

  results <- tibble::tibble(
    Intercept = intercept,
    Slope = slope
  )

  return(results)
  #Refrenced Chat GPT
}

chooseStart <- function(x, y) {


  df <- data.frame(x = x, y = y)

  # Find the row with the minimum x and y values
  min_index<- which.min(df$x + df$y)
  min_val <- df[min_index, ]

  # Find the row with the maximum x and y values
  max_index <- which.max(df$x + df$y)
  max_val <- df[max_index, ]

  rise <- max_val[2] - min_val[2]
  run <- max_val[1] - min_val[1]

  slope <- as.numeric(rise / run)

  sd_x <- sd(x)

  intercept <- as.numeric(min_val[1] - (slope * sd_x))

  return(c(intercept, slope))
}
