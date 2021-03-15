#' T-test function
#'
#' This function performs a t-test on input data and null hypothesis \code{mu}.
#'
#' @param x Numeric vector input of data.
#' @param alternative A character string input that specifies the alternative
#'   hypothesis, defaults to \code{"two.sided"}. Also accepts \code{"less"} and
#'   \code{"greater"}.
#' @param mu Numeric input specifying the null hypothesis of the mean.
#' @keywords inference
#'
#' @return List with the following elements: Test statistic, degrees of freedom,
#'   alternative hypothesis, and P-value.
#'
#' @examples
#' set.seed(111)
#' x <- rnorm(10, mean = 0, sd = 1)
#' my_t.test(x, mu = 0)
#' my_t.test(x, alternative = "less", mu = 1)
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu){
  # Calculate the standard error of the data
  se <- stats::sd(x) / sqrt(length(x))
  # Calculate the test statistic
  test_stat <- (mean(x) - mu) / se
  # Calculate the degrees of freedom
  degf <- length(x) - 1
  # Two sided test...
  if (alternative == "two.sided") {
    # set the alternative hypothesis
    alt = sprintf("true mean is not equal to %g", mu)
    # calculate p value
    p_val = 2 * stats::pt(abs(test_stat), degf, lower.tail = FALSE)
    # One sided test, less than
  } else if (alternative == "less") {
    # set the alternative hypothesis
    alt = sprintf("true mean is less than %g", mu)
    # calculate p value
    p_val = stats::pt(test_stat, degf)
    # One sided test, greater than
  } else if (alternative == "greater") {
    # set the alternative hypothesis
    alt = sprintf("true mean is greater than %g", mu)
    # calculate p value
    p_val = stats::pt(test_stat, degf, lower.tail = FALSE)
  } else {
    # Throw error for bad values of alternative
    stop('alternative parameter must match "two.sided" (default), "less", or "greater"')
  }
  # Create output list
  output <- list("Test statistic" = test_stat,
                 "Degrees of freedom" = degf,
                 "Alternative" = alt,
                 "P-value" = p_val)
  return(output)
}
