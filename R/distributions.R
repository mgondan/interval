#' @title 
#' The Binomial Distribution
#' 
#' @name Binomial
#' 
#' @description
#' For more information, refer to the documentation
#' of [dbinom()], [pbinom()], [qbinom()] from `stats`.
#' 
#' @param x
#' Number of successes
#' 
#' @param p
#' probability 
#' 
#' @param size
#' Number of trials
#' 
#' @param prob
#' Probability of success on each trial 
#' 
#' @param lower.tail
#' logical; if TRUE (default), probabilities are 
#' _P_\[_X&le;x_\], otherwise, _P_\[_X&ge;x_\]
#' 
#' @param ...
#' further arguments
#' 
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' dbinom(...(5, 6), ...(20, 21), ...(0.3, 0.5))
#' pbinom(...(5, 6), ...(20, 21), ...(0.3, 0.5))
#' qbinom(...(0.5, 0.6), ...(20, 21), ...(0.3, 0.5))
#' @export
#' @md
pbinom <- function(x, size, prob, lower.tail = TRUE, ...) {
  .dispatch("pbinom", x, size, prob, lower.tail, ...)
}

.pbinom_interval <- function(x, size, prob, lower.tail = TRUE, ...) {
  .eval("pbinom", x, size, prob, lower.tail)
}

.pbinom_default <- function(x, size, prob, lower.tail = TRUE, ...) {
  stats::pbinom(x, size, prob, lower.tail, ...)
}

#' @rdname Binomial
qbinom <- function(p, size, prob, lower.tail = TRUE, ...) {
  .dispatch("qbinom", p, size, prob, lower.tail, ...)
}

.qbinom_interval <- function(p, size, prob, lower.tail = TRUE, ...) {
  .eval("qbinom", p, size, prob, lower.tail)
}

.qbinom_default <- function(p, size, prob, lower.tail = TRUE, ...) {
  stats::qbinom(p, size, prob, lower.tail, ...)
}

#' @rdname Binomial
dbinom <- function(x, size, prob, ...) {
  .dispatch("dbinom", x, size, prob, ...)
}

.dbinom_interval <- function(x, size, prob, ...) {
  .eval("dbinom", x, size, prob)
}

.dbinom_default <- function(x, size, prob, ...) {
  stats::dbinom(x, size, prob, ...)
}

#' @title 
#' The Normal Distribution
#' 
#' @name Normal
#' 
#' @description
#' For more information, refer to the documentation
#' of [dnorm()], [pnorm()], [qnorm()] from `stats`.
#' 
#' @param q,x
#' vector of quantiles.
#' 
#' @param p
#' vector of probabilities
#' 
#' @param mean
#' vector of means
#' 
#' @param sd
#' vector of standard deviations.
#' 
#' @param lower.tail
#' logical; if TRUE (default), probabilities are 
#' _P_\[_X&le;x_\], otherwise, _P_\[_X&ge;x_\]
#' 
#' @param ...
#' further arguments
#' 
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' pnorm(...(8, 9), ...(10, 11), ...(2, 3))
#' qnorm(...(0.3, 0.4), ...(10, 11), ...(2, 3))
#' dnorm(...(8, 9), ...(10, 11), ...(2, 3))
#' @export
#' @md
pnorm <- function(q, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  .dispatch("pnorm", q, mean, sd, lower.tail, ...)
}

.pnorm_interval <- function(q, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  .eval("pnorm", q, mean, sd, lower.tail)
}

.pnorm_default <- function(q, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  stats::pnorm(q, mean, sd, lower.tail, ...)
}

#' @rdname Normal
qnorm <- function(p, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  .dispatch("qnorm", p, mean, sd, lower.tail, ...)
}

.qnorm_interval <- function(p, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  .eval("qnorm", p, mean, sd, lower.tail)
}

.qnorm_default <- function(p, mean = 0, sd = 1, lower.tail = TRUE, ...) {
  stats::qnorm(p, mean, sd, lower.tail, ...)
}

#' @rdname Normal
dnorm <- function(x, mean = 0, sd = 1, ...) {
  .dispatch("dnorm", x, mean, sd, ...)
}

.dnorm_interval <- function(x, mean = 0, sd = 1, ...) {
  .eval("dnorm", x, mean, sd)
}

.dnorm_default <- function(x, mean = 0, sd = 1, ...) {
  stats::dnorm(x, mean, sd, ...)
}