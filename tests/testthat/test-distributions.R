test_that("pbinom 1", {
  expr <- quote(pbinom_intarith(6, 20, ...(0.3, 0.5)))
  expect_equal(eval(expr), ...(0.05765914916992186, 0.6080098122009241))
})