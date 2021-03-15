#' Linear model function
#'
#' Fits a linear model to input data and outputs a table describing the estimated
#'   coefficients.
#'
#' @param formula Formula class object specifying relationship.
#' @param data Data frame input.
#' @keywords inference
#' @return Data frame including the estimate, standard error, t-value, and
#'   p-value for each coefficient.
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ ., data = mtcars)
#'
#' @export
my_lm <- function(formula, data){
  # Extract model matrix X
  X <- stats::model.matrix(formula, data)
  # Extract model frame object
  model_frame <- stats::model.frame(formula, data)
  # Extract model response Y
  Y <- stats::model.response(model_frame)
  # Solve for coefficients
  estimates <- solve(t(X) %*% X) %*% t(X) %*% Y

  # Define degrees of freedom
  df <- (dplyr::count(model_frame) - length(model_frame)) %>%
    # coerce to numeric (from dataframe??)
    as.numeric()
  # Define variance of the error
  error_var <- 0
  # sum over length of Y
  for (i in 1:length(Y)) {
    error_var <- error_var + ((Y[i]- X[i,] %*% estimates)^2 / df) %>%
      # coerce to numeric (from dataframe??)
      as.numeric()
  }
  # Estimate standard error
  se <- (error_var * solve(t(X) %*% X)) %>%
    sqrt() %>%
    diag()
  # Calculate the test statistic for each estimate
  test_stat <- (estimates - 0) / se

  # Calculate p value for each estimate
  p_vals <- stats::pt(abs(test_stat), df, lower.tail = FALSE) * 2

  # Create output dataframe with coefficient data
  output <- data.frame("Estimate" = estimates,
                       "Std_Error" = se,
                       "t_value" = test_stat,
                       "p_value" = p_vals)
  # Return output dataframe
  return(output)
}
