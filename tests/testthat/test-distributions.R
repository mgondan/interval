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