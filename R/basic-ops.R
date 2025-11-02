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


#' @title 
#' Square root (standard)
#'
#' @description
#' Negative bounds return NaN.
#' 
#' @param x
#' Radicand
#' 
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' sqrt(...(4, 9))
#' sqrt(4 %...% 9)
#' sqrt(interval(4, 9))
#' 
#' @export
#' @md
sqrt.interval <- function(x) {
  .eval("sqrt", x)
}


#' @title 
#' Square root (with cropping)
#'
#' @description
#' Lower bound of the radicand is set to 0 in case it is negative.
#' Otherwise it is equivalent to the `sqrt` function.
#' 
#' @param x
#' Radicand
#' 
#' @return
#' The numeric result as interval or number.
#' 
#' @examples 
#' sqrt0(...(4, 9))
#' sqrt0(-4 %...% 9)
#' sqrt0(interval(4, 9))
#' 
#' @export
#' @md
sqrt0 <- function(x) {
  .eval("sqrt0", x)
}