penguins <- my_penguins %>% tidyr::drop_na()
train <- penguins[,3:6]
cl <- penguins[,1]

test_that("output is list", {
  expect_type(my_knn_cv(train, cl, 1, 5),"list")
})

test_that("not specifying a variable throws an error", {
  expect_error(my_knn_cv(train, cl, 1))
})
