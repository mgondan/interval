#' @title 
#' The Binomial Distribution
#'
#' @rdname binomial
#' 
#' @description
#' For more information, refer to the documentation
#' of [dbinom()], [pbinom()], [qbinom()].
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
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' pbinom_intarith(...(5, 6), ...(20, 21), ...(0.3, 0.5))
#' @export
#' @md
pbinom_intarith <- function(x, size, prob) {
  .eval("pbinom", x, size, prob)
}
