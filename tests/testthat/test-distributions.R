test_that("pbinom 1", {
  expr <- quote(pbinom(6, 20, 0.4))
  expect_equal(rnd(eval(expr)), 0.250011)
})

test_that("pbinom 2", {
  expr <- quote(pbinom(6, 20, 0.4, lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), 0.749989)
})

test_that("pbinom 3", {
  expr <- quote(pbinom(6, 20, 0.4, log.p=TRUE))
  expect_equal(rnd(eval(expr)), -1.386252)
})

test_that("pbinom 4", {
  expr <- quote(pbinom(...(6, 8), 20, 0.3))
  expect_equal(rnd(eval(expr)), ...(0.608009, 0.886669))
})

test_that("pbinom 5", {
  expr <- quote(pbinom(6, ...(20, 21), 0.3))
  expect_equal(rnd(eval(expr)), ...(0.550518, 0.608010))
})

test_that("pbinom 6", {
  expr <- quote(pbinom(6, 20, ...(0.3, 0.5)))
  expect_equal(rnd(eval(expr)), ...(0.057659, 0.608010))
})

test_that("pbinom 7", {
  expr <- quote(pbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4)))
  expect_equal(rnd(eval(expr)), ...(0.158443, 0.886669))
})

test_that("pbinom 8", {
  expr <- quote(pbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(0.113331, 0.841557))
})

test_that("pbinom 9", {
  expr <- quote(pbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4), log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(-1.842357, -0.120284))
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

test_that("qbinom 6", {
  expr <- quote(qbinom(...(0.5, 0.7), ...(20, 22), ...(0.2, 0.4), lower.tail=FALSE))
  expect_equal(eval(expr), ...(3.0, 9.0))
})

test_that("dbinom 1", {
  expr <- quote(dbinom(6, 20, 0.4))
  expect_equal(rnd(eval(expr)), 0.124412)
})

test_that("dbinom 2", {
  expr <- quote(dbinom(...(6, 8), 20, 0.3))
  expect_equal(rnd(eval(expr)), ...(0.114396, 0.191639))
})

test_that("dbinom 3", {
  expr <- quote(dbinom(6, ...(20, 21), 0.3))
  expect_equal(rnd(eval(expr)), ...(0.187806, 0.191639))
})

test_that("dbinom 4", {
  expr <- quote(dbinom(6, 20, ...(0.3, 0.5)))
  expect_equal(rnd(eval(expr)), ...(0.036964, 0.191639))
})

test_that("dbinom 5", {
  expr <- quote(dbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4)))
  expect_equal(rnd(eval(expr)), ...(0.086217, 0.191639))
})

test_that("pnorm 1", {
  expr <- quote(pnorm(9, 10, 2))
  expect_equal(rnd(eval(expr)), 0.308538)
})

test_that("pnorm 2", {
  expr <- quote(pnorm(...(8, 9), 10, 2))
  expect_equal(rnd(eval(expr)), ...(0.158655, 0.308538))
})

test_that("pnorm 3", {
  expr <- quote(pnorm(9, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(0.158655, 0.308538))
})

test_that("pnorm 4", {
  expr <- quote(pnorm(9, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.308537, 0.369442))
})

test_that("pnorm 5", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.066807, 0.369442))
})

test_that("pnorm 6", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(0.630558, 0.933193))
})

test_that("qnorm 1", {
  expr <- quote(qnorm(0.3, 10, 2))
  expect_equal(rnd(eval(expr)), 8.951199)
})

test_that("qnorm 2", {
  expr <- quote(qnorm(...(0.3, 0.4), 10, 2))
  expect_equal(rnd(eval(expr)), ...(8.951198, 9.493306))
})

test_that("qnorm 3", {
  expr <- quote(qnorm(0.3, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(8.951198, 9.951199))
})

test_that("qnorm 4", {
  expr <- quote(qnorm(0.3, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(8.426798, 8.951199))
})

test_that("qnorm 5", {
  expr <- quote(qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(8.426798, 10.493306))
})

test_that("qnorm 6", {
  expr <- quote(qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(10.506694, 12.573202))
})

test_that("dnorm 1", {
  expr <- quote(dnorm(8, 10, 2))
  expect_equal(rnd(eval(expr)), 0.120985)
})

test_that("dnorm 2", {
  expr <- quote(dnorm(...(8, 9), 10, 2))
  expect_equal(rnd(eval(expr)), ...(0.120985, 0.176033))
})

test_that("dnorm 3", {
  expr <- quote(dnorm(8, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(0.064758, 0.120986))
})

test_that("dnorm 4", {
  expr <- quote(dnorm(8, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.080656, 0.159725))
})

test_that("dnorm 5", {
  expr <- quote(dnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.043172, 0.188692))
})