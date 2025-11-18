#' @title 
#' The Binomial Distribution
#'
#' @description
#' For more information, refer to the documentation
#' of [dbinom()], [pbinom()], [qbinom()] from `stats`.
#' 
#' @param x
#' Number of successes
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
#' pbinom(...(5, 6), ...(20, 21), ...(0.3, 0.5))
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
