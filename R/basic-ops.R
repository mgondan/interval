#' @title 
#' Addition
#'
#' @param x
#' Left operand
#'
#' @param y
#' Right operand
#'
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' ...(1, 2) + ...(3, 4)
#' 1 %...% 2 + 3 %...% 4
#' interval(1, 2) + interval(3, 4)
#' 
#' @export
#' @md
"+.interval" <- function(x, y) {
  .eval("+", x, y)
}
