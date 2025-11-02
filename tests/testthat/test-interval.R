test_that("check class", {
  expect_s3_class(interval(1, 2), "interval")
})

test_that("check bounds", {
  actual <- interval(1, 2)
  expect_equal(actual$l, 1)
  expect_equal(actual$u, 2)
})

test_that("non-numeric argument", {
  expect_error(interval("a", 2))
})

test_that("invalid values for bounds", {
  expect_error(interval(1, 0.9))
})

test_that("infix operator", {
  actual <- 3%...%4
  expect_s3_class(actual, "interval")
  expect_equal(actual$l, 3)
  expect_equal(actual$u, 4)
})

test_that("dots operator", {
  actual <- ...(2, 3)
  expect_s3_class(actual, "interval")
  expect_equal(actual$l, 2)
  expect_equal(actual$u, 3)
})

test_that("NaN argument right", {
  actual <- (interval(NaN, 2))
  expect_equal(actual, NaN)
})

test_that("NaN argument left", {
  actual <- (interval(1, NaN))
  expect_equal(actual, NaN)
})

test_that("NaN argument both", {
  actual <- (interval(NaN, NaN))
  expect_equal(actual, NaN)
})
