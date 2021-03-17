#' Random forest cross-validation function
#'
#' Using the palmerpenguins data, predicts body mass with covariates bill length,
#'   bill depth, and flipper length by the random forest algorithm and performs
#'   cross-validation on the model.
#'
#' @param k Integer representing the number of folds.
#'
#' @keywords inference prediction
#' @return A numeric with the cross-validation error.
#'
#' @importFrom tidyr drop_na
#' @importFrom randomForest randomForest
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k){
  # Trim penguins dataset to only variables we want
  pen <- StatsTools::my_penguins[,3:6] %>% tidyr::drop_na()
  # Create vector to split data into k random parts
  set.seed(303)
  folds <- sample(rep(1:k, length = nrow(pen)))
  # Add vector to data
  pen$folds <- folds
  # Create empty MSE vector
  mse_vec <- c()
  # Iterate over k_cv folds to cross-validate knn model
  for (i in 1:k) {
    # Select training data, remove fold classification
    data_train <- pen %>% dplyr::filter(folds != i) %>% dplyr::select(-folds)
    # Select test data, remove fold classification
    data_test <- pen %>% dplyr::filter(folds == i) %>% dplyr::select(-folds)
    # Train model
    rf_model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                             data = data_train, ntree = 100)
    # Predict body mass with model
    rf_predict <- stats::predict(rf_model, data_test[, -4])
    # Calculate mean squared error
    mse <- mean((data_test$body_mass_g - rf_predict)^2)
    # Add MSE of this fold to the MSE vector
    mse_vec[i] <- mse
  }
  # Return average MSE
  return(mean(mse_vec))
}
