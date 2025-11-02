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


#' @title 
#' Subtraction
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
#' ...(3, 4) - ...(1, 2)
#' 1 %...% 2 - 3 %...% 4
#' interval(1, 2) - interval(3, 4)
#' 
#' @export
#' @md
"-.interval" <- function(x, y) {
  .eval("-", x, y)
}


#' @title 
#' Multiplication
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
#' ...(3, 4) * ...(1, 2)
#' 1 %...% 2 * 3 %...% 4
#' interval(1, 2) * interval(3, 4)
#' 
#' @export
#' @md
"*.interval" <- function(x, y) {
  .eval("*", x, y)
}


#' @title 
#' Division
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
#' ...(3, 4) / ...(1, 2)
#' 1 %...% 2 / 3 %...% 4
#' interval(1, 2) / interval(3, 4)
#' 
#' @export
#' @md
"/.interval" <- function(x, y) {
  .eval("/", x, y)
}


#' @title 
#' Power
#'
#' @param x
#' Base
#'
#' @param y
#' Exponent as number
#'
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' ...(3, 4) ^ 2
#' 3 %...% 4 ^ 3
#' interval(3, 4) ^ 2
#' 
#' @export
#' @md
"^.interval" <- function(x, y) {
  .eval("^", x, y)
}


#' @title 
#' Exponential
#'
#' @param x
#' Exponent
#'
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' exp(...(3, 4))
#' exp(3 %...% 4)
#' exp(interval(3, 4))
#' 
#' @export
#' @md
exp.interval <- function(x) {
  .eval("exp", x)
}