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
  expr <- quote(qbinom(0.9, 20, 0.2))
  expect_equal(eval(expr), 6.0)
})

test_that("qbinom 2", {
  expr <- quote(qbinom(0.9, 20, 0.2, lower.tail=FALSE))
  expect_equal(eval(expr), 2.0)
})

test_that("qbinom 3", {
  expr <- quote(qbinom(-0.11, 20, 0.2, log.p=TRUE))
  expect_equal(eval(expr), 6.0)
})

test_that("qbinom 4", {
  expr <- quote(qbinom(...(0.5, 0.7), 20, 0.2))
  expect_equal(eval(expr), ...(4.0, 5.0))
})

test_that("qbinom 5", {
  expr <- quote(qbinom(0.5, ...(20, 24), 0.2))
  expect_equal(eval(expr), ...(4.0, 5.0))
})

test_that("qbinom 6", {
  expr <- quote(qbinom(0.5, 20, ...(0.2, 0.4)))
  expect_equal(eval(expr), ...(4.0, 8.0))
})

test_that("qbinom 7", {
  expr <- quote(qbinom(...(0.5, 0.7), ...(20, 22), ...(0.2, 0.4)))
  expect_equal(eval(expr), ...(4.0, 10.0))
})

test_that("qbinom 8", {
  expr <- quote(qbinom(...(0.5, 0.7), ...(20, 22), ...(0.2, 0.4), lower.tail=FALSE))
  expect_equal(eval(expr), ...(3.0, 9.0))
})

test_that("qbinom 9", {
  expr <- quote(qbinom(...(-0.69, -0.36), ...(20, 22), ...(0.2, 0.4), log.p=TRUE))
  expect_equal(eval(expr), ...(4.0, 10.0))
})

test_that("dbinom 1", {
  expr <- quote(dbinom(6, 20, 0.4))
  expect_equal(rnd(eval(expr)), 0.124412)
})

test_that("dbinom 2", {
  expr <- quote(dbinom(6, 20, 0.4, log=TRUE))
  expect_equal(rnd(eval(expr)), -2.084159)
})

test_that("dbinom 3", {
  expr <- quote(dbinom(...(6, 8), 20, 0.3))
  expect_equal(rnd(eval(expr)), ...(0.114396, 0.191639))
})

test_that("dbinom 4", {
  expr <- quote(dbinom(6, ...(20, 21), 0.3))
  expect_equal(rnd(eval(expr)), ...(0.187806, 0.191639))
})

test_that("dbinom 5", {
  expr <- quote(dbinom(6, 20, ...(0.3, 0.5)))
  expect_equal(rnd(eval(expr)), ...(0.036964, 0.191639))
})

test_that("dbinom 6", {
  expr <- quote(dbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4)))
  expect_equal(rnd(eval(expr)), ...(0.086217, 0.191639))
})

test_that("dbinom 7", {
  expr <- quote(dbinom(...(6, 8), ...(20, 22), ...(0.3, 0.4), log=TRUE))
  expect_equal(rnd(eval(expr)), ...(-2.450885, -1.652141))
})

test_that("pnorm 1", {
  expr <- quote(pnorm(9, 10, 2))
  expect_equal(rnd(eval(expr)), 0.308538)
})

test_that("pnorm 2", {
  expr <- quote(pnorm(9, 10, 2, lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), 0.691462)
})

test_that("pnorm 3", {
  expr <- quote(pnorm(9, 10, 2, log.p=TRUE))
  expect_equal(rnd(eval(expr)), -1.175912)
})

test_that("pnorm 4", {
  expr <- quote(pnorm(...(8, 9), 10, 2))
  expect_equal(rnd(eval(expr)), ...(0.158655, 0.308538))
})

test_that("pnorm 5", {
  expr <- quote(pnorm(9, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(0.158655, 0.308538))
})

test_that("pnorm 6", {
  expr <- quote(pnorm(9, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.308537, 0.369442))
})

test_that("pnorm 7", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.066807, 0.369442))
})

test_that("pnorm 8", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(0.630558, 0.933193))
})

test_that("pnorm 9", {
  expr <- quote(pnorm(...(8, 9), ...(10, 11), ...(2, 3), log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(-2.705945, -0.995763))
})

test_that("qnorm 1", {
  expr <- quote(qnorm(0.3, 10, 2))
  expect_equal(rnd(eval(expr)), 8.951199)
})

test_that("qnorm 2", {
  expr <- quote(qnorm(0.3, 10, 2, lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), 11.048801)
})

test_that("qnorm 3", {
  expr <- quote(qnorm(-1.2, 10, 2, log.p=TRUE))
  expect_equal(rnd(eval(expr)), 8.958062)
})

test_that("qnorm 4", {
  expr <- quote(qnorm(...(0.3, 0.4), 10, 2))
  expect_equal(rnd(eval(expr)), ...(8.951198, 9.493306))
})

test_that("qnorm 5", {
  expr <- quote(qnorm(0.3, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(8.951198, 9.951199))
})

test_that("qnorm 6", {
  expr <- quote(qnorm(0.3, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(8.426798, 8.951199))
})

test_that("qnorm 7", {
  expr <- quote(qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(8.426798, 10.493306))
})

test_that("qnorm 8", {
  expr <- quote(qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(10.506694, 12.573202))
})

test_that("qnorm 9", {
  expr <- quote(qnorm(...(-1.2, -0.92), ...(10, 11), ...(2, 3), log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(8.437093, 10.485636))
})

test_that("dnorm 1", {
  expr <- quote(dnorm(8, 10, 2))
  expect_equal(rnd(eval(expr)), 0.120985)
})

test_that("dnorm 2", {
  expr <- quote(dnorm(8, 10, 2, log=TRUE))
  expect_equal(rnd(eval(expr)), -2.112086)
})

test_that("dnorm 3", {
  expr <- quote(dnorm(...(8, 9), 10, 2))
  expect_equal(rnd(eval(expr)), ...(0.120985, 0.176033))
})

test_that("dnorm 4", {
  expr <- quote(dnorm(8, ...(10, 11), 2))
  expect_equal(rnd(eval(expr)), ...(0.064758, 0.120986))
})

test_that("dnorm 5", {
  expr <- quote(dnorm(8, 10, ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.080656, 0.159725))
})

test_that("dnorm 6", {
  expr <- quote(dnorm(...(8, 9), ...(10, 11), ...(2, 3)))
  expect_equal(rnd(eval(expr)), ...(0.043172, 0.188692))
})

test_that("dnorm 7", {
  expr <- quote(dnorm(...(8, 9), ...(10, 11), ...(2, 3), log=TRUE))
  expect_equal(rnd(eval(expr)), ...(-3.142551, -1.667641))
})

test_that("pt 1", {
  expr <- quote(pt(2, 10))
  expect_equal(rnd(eval(expr)), 0.963306)
})

test_that("pt 2", {
  expr <- quote(pt(2, 10, lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), 0.036694)
})

test_that("pt 3", {
  expr <- quote(pt(2, 10, 1))
  expect_equal(rnd(eval(expr)), 0.807612)
})

test_that("pt 4", {
  expr <- quote(pt(...(2, 3), 10))
  expect_equal(rnd(eval(expr)), ...(0.963305, 0.993329))
})

test_that("pt 5", {
  expr <- quote(pt(2, ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.963305, 0.964599))
})

test_that("pt 6", {
  expr <- quote(pt(...(2, 3), ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.963305, 0.993961))
})

test_that("pt 7", {
  expr <- quote(pt(...(2, 3), ...(10, 11), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(0.006039, 0.036695))
})

test_that("pt 8", {
  expr <- quote(pt(...(2, 3), ...(10, 11), log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(-0.037385, -0.006058))
})

test_that("pt 9", {
  expr <- quote(pt(...(2, 3), ...(10, 11), lower.tail=FALSE, log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(-5.109365, -3.305141))
})

test_that("qt 1", {
  expr <- quote(qt(0.6, 10))
  expect_equal(rnd(eval(expr)), 0.260185)
})

test_that("qt 2", {
  expr <- quote(qt(0.6, 10, lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), -0.260185)
})

test_that("qt 3", {
  expr <- quote(qt(0.6, 10, 1))
  expect_equal(rnd(eval(expr)), 1.296262)
})

test_that("qt 4", {
  expr <- quote(qt(...(0.6, 0.7), 10))
  expect_equal(rnd(eval(expr)), ...(0.260184, 0.541529))
})

test_that("qt 5", {
  expr <- quote(qt(0.6, ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.259555, 0.260185))
})

test_that("qt 6", {
  expr <- quote(qt(...(0.6, 0.7), ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.259555, 0.541529))
})

test_that("qt 7", {
  expr <- quote(qt(...(0.6, 0.7), ...(10, 11), ncp=1))
  expect_equal(rnd(eval(expr)), ...(1.292224, 1.596458))
})

test_that("qt 8", {
  expr <- quote(qt(...(0.6, 0.7), ...(10, 11), lower.tail=FALSE))
  expect_equal(rnd(eval(expr)), ...(-0.541529, -0.259555))
})

test_that("qt 9", {
  expr <- quote(qt(...(-0.5108, -0.3566), ...(10, 11), log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(0.259596, 0.541687))
})

test_that("qt 10", {
  expr <- quote(qt(...(-0.5108, -0.3566), ...(10, 11), lower.tail=FALSE, log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(-0.541687, -0.259596))
})

test_that("qt 11", {
  expr <- quote(qt(...(-0.5108, -0.3566), ...(10, 11), ncp=1, lower.tail=FALSE, log.p=TRUE))
  expect_equal(rnd(eval(expr)), ...(0.483614, 0.762011))
})

test_that("dt 1", {
  expr <- quote(dt(2, 10))
  expect_equal(rnd(eval(expr)), 0.061146)
})

test_that("dt 2", {
  expr <- quote(dt(2, 10, log=TRUE))
  expect_equal(rnd(eval(expr)), -2.794495)
})

test_that("dt 3", {
  expr <- quote(dt(2, 10, 1))
  expect_equal(rnd(eval(expr)), 0.225424)
})

test_that("dt 4", {
  expr <- quote(dt(...(2, 3), 10))
  expect_equal(rnd(eval(expr)), ...(0.011400, 0.061146))
})

test_that("dt 5", {
  expr <- quote(dt(2, ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.060654, 0.061146))
})

test_that("dt 6", {
  expr <- quote(dt(...(2, 3), ...(10, 11)))
  expect_equal(rnd(eval(expr)), ...(0.010795, 0.061146))
})

test_that("dt 7", {
  expr <- quote(dt(...(2, 3), ...(10, 11), log=TRUE))
  expect_equal(rnd(eval(expr)), ...(-4.528657, -2.794494))
})

test_that("dt 8", {
  expr1 <- quote(dt(...(2, 3), ...(10, 11), ncp=0))
  expr2 <- quote(dt(...(2, 3), ...(10, 11)))
  expect_equal(rnd(eval(expr1)), rnd(eval(expr2)))
})

test_that("dt 9", {
  expr <- quote(dt(...(2, 3), ...(10, 11), ncp=1))
  expect_equal(rnd(eval(expr)), ...(0.071435, 0.226811))
})

test_that("dt 10", {
  expr <- quote(dt(...(2, 3), ...(10, 11), ncp=1, log=TRUE))
  expect_equal(rnd(eval(expr)), ...(-2.638958, -1.483638))
})