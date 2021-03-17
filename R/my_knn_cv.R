#' K-nearest-neighbors cross-validation function
#'
#' Predicts the class of the input data using k-nearest-neighbors and performs
#'   cross-validation to determine the misclassification error of the model.
#'
#' @param train Data frame input of training data.
#' @param cl Vector input of true class values for the training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#'
#' @keywords inference prediction
#' @return List including a vector of class predictions for all observations and
#'   a numeric with the cross-validation misclassification error.
#'
#' @importFrom tidyr drop_na
#'
#' @examples
#' penguins <- my_penguins %>% tidyr::drop_na()
#' train <- penguins[,3:6]
#' cl <- penguins[,1]
#' my_knn_cv(train, cl, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Create vector to split data into k random parts
  #browser()
  set.seed(303)
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  # Add vector to data
  train$folds <- folds
  cl <- as.data.frame(cl)
  cl$folds <- folds
  # Create empty misclassification vector
  misclass <- rep(NA, 5)
  # Iterate over k_cv folds to cross-validate knn model
  for (i in 1:k_cv) {
    # Select training data, remove fold classification
    data_train <- train %>% dplyr::filter(folds != i) %>% dplyr::select(-folds)
    # Select test data, remove fold classification
    data_test <- train %>% dplyr::filter(folds == i) %>% dplyr::select(-folds)
    # Filter classes to only include training data
    cl_train <- cl %>% dplyr::filter(folds != i) %>% dplyr::pull(-folds)
    # Filter classes to only include test data
    cl_test <- cl %>% dplyr::filter(folds == i) %>% dplyr::pull(-folds)
    # Predict class of test data
    knn_predict <- class::knn(data_train, data_test, cl_train, k = k_nn)
    # Calculate the misclassification rate of knn
    misclass[i] <- 1 - mean(knn_predict == cl_test)
  }
  # Train on full data
  pred <- class::knn(train, train, cl[,1], k = k_nn)
  # Create list with results and return
  result <- list("class" = pred,
                 "cv_err" = mean(misclass))
  return(result)
}
