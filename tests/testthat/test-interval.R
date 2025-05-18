test_that("basic call", {
  expect_equal(interval("1...2 + 2...3"), "3...5")
})

test_that("basic R function", {
  expect_equal(interval("round(pnorm(0.5...0.6), 4)"), "0.6914...0.7258")
})
