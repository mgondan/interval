test_that("addition1", {
  expr <- quote(...(4, 6) + ...(6, 9))
  expect_equal(eval(expr), ...(10, 15))
})

test_that("addition2", {
  expr <- quote(1 + ...(6, 9))
  expect_equal(eval(expr), ...(7, 10))
})

test_that("addition3", {
  expr <- quote(...(6, 9) + 2)
  expect_equal(eval(expr), ...(8, 11))
})

test_that("addition4", {
  expr <- quote(...(6, 9) + 2 + ...(7, 9) + 3)
  expect_equal(eval(expr), ...(18, 23))
})