set.seed(111)
x <- rnorm(10, mean = 0, sd = 1)

test_that("output is list", {
  expect_type(my_t.test(x, mu = 0),"list")
  expect_type(my_t.test(x, alternative = "less", mu = 0),"list")
  expect_type(my_t.test(x, alternative = "greater", mu = 0),"list")

})

test_that("incorrect value of alternative throws an error", {
  expect_error(my_t.test(x, alternative = "more", mu = 1))
})
