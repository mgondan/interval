test_that(".eval", {
  x <- interval(1, 2)
  expected <- interval(4, 5)
  expect_equal(.eval("+", x, 3), expected)
})

test_that(".arg2char", {
  x <- interval(1, 2)
  expect_equal(.arg2char(x), "1...2")
  expect_equal(.arg2char(1), "1")
  expect_equal(.arg2char(0.1), "0.1")
})

test_that(".is_interval", {
  x <- interval(1, 2)
  expect_true(.is_interval(x))
  expect_false(.is_interval(1))
  expect_false(.is_interval("a"))
})