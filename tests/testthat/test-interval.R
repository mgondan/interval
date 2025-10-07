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

