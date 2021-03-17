test_that("output is numeric", {
  expect_type(my_rf_cv(5), "double")
})

test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("pi"))
  expect_error(my_rf_cv(matrix(1:9, nrow = 3, ncol = 3)))
})
