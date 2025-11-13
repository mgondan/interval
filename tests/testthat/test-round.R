test_that("round1", {
  expr <- quote(round(...(1.582, 2.168), digits=2))
  expect_equal(eval(expr), ...(1.58, 2.17))
})

test_that("round2interval_1", {
  expr <- quote(round2interval(2.135, digits=2))
  expect_equal(eval(expr), ...(2.13, 2.14))
})

test_that("round2interval_2", {
  expr <- quote(round2interval(2.1, digits=2))
  expect_equal(eval(expr), ...(2.09, 2.11))
})
