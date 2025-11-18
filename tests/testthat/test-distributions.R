test_that("pbinom 1", {
  expr <- quote(pbinom(6, 20, 0.4))
  expect_equal(eval(expr), 0.25001067193782217)
})

test_that("pbinom 2", {
  expr <- quote(pbinom(...(6, 8), 20, 0.3))
  expect_equal(eval(expr), ...(0.6080098122009241, 0.8866685371230216))
})

test_that("pbinom 3", {
  expr <- quote(pbinom(6, ...(20, 21), 0.3))
  expect_equal(eval(expr), ...(0.5505181173748914, 0.6080098122009241))
})

test_that("pbinom 4", {
  expr <- quote(pbinom(6, 20, ...(0.3, 0.5)))
  expect_equal(eval(expr), ...(0.05765914916992186, 0.6080098122009241))
})

test_that("pbinom 5", {
  expr <- quote(pbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4)))
  expect_equal(eval(expr), ...(0.15844366131574084, 0.8866685371230216))
})
