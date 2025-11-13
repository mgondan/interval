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

test_that("subtraction1", {
  expr <- quote(...(4, 6) - ...(2, 3))
  expect_equal(eval(expr), ...(1, 4))
})

test_that("subtraction2", {
  expr <- quote(1 - ...(6, 9))
  expect_equal(eval(expr), ...(-8, -5))
})

test_that("subtraction3", {
  expr <- quote(...(6, 9) - 2)
  expect_equal(eval(expr), ...(4, 7))
})

test_that("subtraction4", {
  expr <- quote(...(10, 12) - 2 - ...(2, 3) - 3)
  expect_equal(eval(expr), ...(2, 5))
})

test_that("multiplication1", {
  expr <- quote(...(4, 6) * ...(2, 3))
  expect_equal(eval(expr), ...(8, 18))
})

test_that("multiplication2", {
  expr <- quote(2 * ...(6, 9))
  expect_equal(eval(expr), ...(12, 18))
})

test_that("multiplication3", {
  expr <- quote(...(6, 9) * 2)
  expect_equal(eval(expr), ...(12, 18))
})

test_that("multiplication4", {
  expr <- quote(...(1, 2) * 2 * ...(2, 3) * 3)
  expect_equal(eval(expr), ...(12, 36))
})

test_that("division1", {
  expr <- quote(...(4, 6) / ...(4, 5))
  expect_equal(eval(expr), ...(0.8, 1.5))
})

test_that("division2", {
  expr <- quote(2 / ...(4, 5))
  expect_equal(eval(expr), ...(0.4, 0.5))
})

test_that("division3", {
  expr <- quote(...(6, 9) / 2)
  expect_equal(eval(expr), ...(3, 4.5))
})

test_that("division4", {
  expr <- quote(...(5, 6) / 2 / ...(1, 2) / 2)
  expect_equal(eval(expr), ...(0.625, 1.5))
})

test_that("power1", {
  expr <- quote(...(2, 3) ^ 2)
  expect_equal(eval(expr), ...(4, 9))
})

test_that("exp1", {
  expr <- quote(exp(...(2, 3)))
  expect_equal(eval(expr), ...(7.38905609893065, 20.085536923187668))
})

test_that("sqrt_1", {
  expr <- quote(sqrt(...(4, 9)))
  expect_equal(eval(expr), ...(2, 3))
})

test_that("sqrt_2", {
  expr <- quote(sqrt(...(-2, 9)))
  expect_equal(eval(expr), NaN)
})

test_that("sqrt_3", {
  expr <- quote(sqrt(...(-2, 0)))
  expect_equal(eval(expr), NaN)
})

test_that("sqrt_4", {
  expr <- quote(sqrt(...(-2, -1)))
  expect_equal(eval(expr), NaN)
})

test_that("sqrt0_1", {
  expr <- quote(sqrt0(...(4, 9)))
  expect_equal(eval(expr), ...(2, 3))
})

test_that("sqrt0_2", {
  expr <- quote(sqrt0(...(-2, 9)))
  expect_equal(eval(expr), ...(0, 3))
})

test_that("sqrt0_3", {
  expr <- quote(sqrt0(...(-2, 0)))
  expect_equal(eval(expr), 0)
})

test_that("sqrt0_4", {
  expr <- quote(sqrt0(...(-2, -1)))
  expect_equal(eval(expr), NaN)
})

test_that("abs1", {
  expr <- quote(abs(...(-2, -1)))
  expect_equal(eval(expr), ...(1, 2))
})

test_that("abs2", {
  expr <- quote(abs(...(1, 2)))
  expect_equal(eval(expr), ...(1, 2))
})

test_that("abs3", {
  expr <- quote(abs(...(-1, 2)))
  expect_equal(eval(expr), ...(0.0, 2))
})

test_that("abs4", {
  expr <- quote(abs(...(-2, 1)))
  expect_equal(eval(expr), ...(0.0, 2))
})

test_that("unary plus 1", {
  expr <- quote(+(...(-2, -1)))
  expect_equal(eval(expr), ...(-2, -1))
})

test_that("unary minus 1", {
  expr <- quote(-(...(-2, -1)))
  expect_equal(eval(expr), ...(1, 2))
})

test_that("sin1", {
  expr <- quote(sin(...(2, 9)))
  expect_equal(eval(expr), ...(-1, 1))
})
