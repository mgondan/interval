test_that("basic call number", {
  expect_equal(intarith("1 + 2"), 3)
})

test_that("basic call interval", {
  expected <- interval(3, 5)
  expect_equal(intarith("1...2 + 2...3"), expected)
})

test_that("basic R function", {
  expected <- interval(0.6914, 0.7258)
  expect_equal(intarith("round(pnorm(0.5...0.6), 4)"), expected)
})

test_that("invalid input", {
  expect_error((intarith("1 + ")))
})
