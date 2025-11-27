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

test_that("qbinom 1", {
  expr <- quote(qbinom(0.5, 20, 0.2))
  expect_equal(eval(expr), 4.0)
})

test_that("qbinom 2", {
  expr <- quote(qbinom(...(0.5, 0.7), 20, 0.2))
  expect_equal(eval(expr), ...(4.0, 5.0))
})

test_that("qbinom 3", {
  expr <- quote(qbinom(0.5, ...(20, 24), 0.2))
  expect_equal(eval(expr), ...(4.0, 5.0))
})

test_that("qbinom 4", {
  expr <- quote(qbinom(0.5, 20, ...(0.2, 0.4)))
  expect_equal(eval(expr), ...(4.0, 8.0))
})

test_that("qbinom 5", {
  expr <- quote(qbinom(...(0.5, 0.7), ...(20, 22), ...(0.2, 0.4)))
  expect_equal(eval(expr), ...(4.0, 10.0))
})

test_that("dbinom 1", {
  expr <- quote(dbinom(6, 20, 0.4))
  expect_equal(eval(expr), 0.1244117)
})

test_that("dbinom 2", {
  expr <- quote(dbinom(...(6, 8), 20, 0.3))
  expect_equal(eval(expr), ...(0.11439673970486122, 0.19163898275344257))
})

test_that("dbinom 3", {
  expr <- quote(dbinom(6, ...(20, 21), 0.3))
  expect_equal(eval(expr), ...(0.1878062030983737, 0.19163898275344257))
})

test_that("dbinom 4", {
  expr <- quote(dbinom(6, 20, ...(0.3, 0.5)))
  expect_equal(eval(expr), ...(0.03696441650390632, 0.19163898275344257))
})

test_that("dbinom 5", {
  expr <- quote(dbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4)))
  expect_equal(eval(expr), ...(0.08621730755584592, 0.19163898275344257))
})

test_that("pnorm 1", {
  expr <- quote(pnorm(9, 10, 2))
  expect_equal(eval(expr), 0.308537539)
})

test_that("pnorm 2", {
  expr <- quote(pnorm(...(8, 9), 10, 2))
  expect_equal(eval(expr), ...(0.15865525393145705, 0.30853753872598694))
})

test_that("pnorm 3", {
  expr <- quote(pnorm(9, ...(10, 11), 2))
  expect_equal(eval(expr), ...(0.15865525393145705, 0.30853753872598694))
})

test_that("pnorm 4", {
  expr <- quote(pnorm(9, 10, ...(2, 3)))
  expect_equal(eval(expr), ...(0.30853753872598694, 0.369441340181763674))
})

test_that("pnorm 5", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(eval(expr), ...(0.06680720126885807, 0.36944134018176367))
})

test_that("qnorm 1", {
  expr <- quote(qnorm(0.3, 10, 2))
  expect_equal(eval(expr), 8.951199)
})

test_that("qnorm 2", {
  expr <- quote(qnorm(...(0.3, 0.4), 10, 2))
  expect_equal(eval(expr), ...(8.951198974583919, 9.4933057937284))
})

test_that("qnorm 3", {
  expr <- quote(qnorm(0.3, ...(10, 11), 2))
  expect_equal(eval(expr), ...(8.951198974583919, 9.951198974583919))
})

test_that("qnorm 4", {
  expr <- quote(qnorm(0.3, 10, ...(2, 3)))
  expect_equal(eval(expr), ...(8.426798461875878, 8.951198974583919))
})

test_that("qnorm 5", {
  expr <- quote(qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3)))
  expect_equal(eval(expr), ...(8.426798461875878, 10.4933057937284))
})

test_that("dnorm 1", {
  expr <- quote(dnorm(8, 10, 2))
  expect_equal(eval(expr), 0.120985362)
})

test_that("dnorm 2", {
  expr <- quote(dnorm(...(8, 9), 10, 2))
  expect_equal(eval(expr), ...(0.12098536225957168, 0.17603266338214976))
})

test_that("dnorm 3", {
  expr <- quote(dnorm(8, ...(10, 11), 2))
  expect_equal(eval(expr), ...(0.06475879783294587, 0.12098536225957168))
})

test_that("dnorm 4", {
  expr <- quote(dnorm(8, 10, ...(2, 3)))
  expect_equal(eval(expr), ...(0.08065690817304778, 0.15972400276117613))
})

test_that("dnorm 5", {
  expr <- quote(dnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(eval(expr), ...(0.04317253188863058, 0.18869161384649658))
})