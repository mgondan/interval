test_that("print", {
  x <- interval(1, 2)
  expect_output(print(x), "1...2")
})
