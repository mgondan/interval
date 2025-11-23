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
