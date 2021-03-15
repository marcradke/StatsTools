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
#' @examples
#' train <- Puromycin[,1:2]
#' cl <- Puromycin[,3]
#' my_knn_cv(train, cl, 2, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Create vector to split data into k random parts
  set.seed(303)
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  # Add vector to data
  train$folds <- folds
  cl$folds <- folds
  # Change cl to dataframe
  cl <- data.frame(cl)
  # Create empty prediction vector
  pred <- c()
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
    # Add predictions for this fold to full prediction matrix
    pred[folds == i] <- as.character(knn_predict)
    # Calculate the misclassification rate of knn
    misclass[i] <- 1 - mean(knn_predict == cl_test)
  }
  # Create list with results and return
  result <- list("class" = pred,
                 "cv_err" = mean(misclass))
  return(result)
}
