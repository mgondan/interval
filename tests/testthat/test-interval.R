test_that("basic call", {
  expect_equal(interval("1...2 + 2...3"), "3...5")
})
