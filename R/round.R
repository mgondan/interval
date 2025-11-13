#' @title 
#' Round
#' 
#' @param x
#' An interval
#' 
#' @param digits
#' The number of digits to keep after the decimal comma
#' 
#' @return
#' The numeric result as interval.
#' 
#' @examples 
#' round(...(4, 9))
#' round(-4 %...% 9)
#' round(interval(4, 9))
#' 
#' @export
#' @md
round.interval <- function(x, digits=getOption("digits")) {
  .eval("round", x, digits)
}


#' @title 
#' Round to interval
#' 
#' @description 
#' The number is converted to a degenerated interval before rounding.
#' This ensures that the result is still an interval. 
#' 
#' @param x
#' A number
#' 
#' @param digits
#' The number of digits to keep after the decimal comma
#' 
#' @return
#' The numeric result as interval.
#' 
#' @examples 
#' round2interval(4.166, 2)
#' 
#' @export
#' @md
round2interval <- function(x, digits=getOption("digits")) {
  .eval("round", x, digits)
}
