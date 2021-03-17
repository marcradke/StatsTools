data(mtcars)

test_that("output is data frame", {
  expect_s3_class(my_lm(mpg ~ ., data = mtcars), "data.frame")
})

test_that("non-formula input throws an error", {
  expect_error(my_lm(mpg = ., data = mtcars))
})
