test_that("round1", {
  expr <- quote(round(...(1.582, 2.168), digits=2))
  expect_equal(eval(expr), ...(1.58, 2.17))
})

test_that("round2interval", {
  expr <- quote(round2interval(...(1.582, 2.168), digits=2))
  expect_equal(round2interval(2.135, digits=2), ...(2.13, 2.14))
})
