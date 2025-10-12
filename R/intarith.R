#' @title 
#' Evaluate expression
#'
#' @description
#' Evaluate an expression in interval arithmetic without having to create
#' interval objects. Intervals can be written as "...(a, b)" or "a...b".
#'
#' @param expr
#' An arithmetic expression as string.
#'
#' @param flags
#' Optional flags.
#'
#' @param env
#' The environment to be used for the evaluation.
#'
#' @return
#' The numeric result of the expression as interval.
#' 
#' @examples 
#' intarith("1...2 + ...(3, 4) + 5")
#' 
#' @export
#' @md
intarith <- function(expr, flags = NULL, env = globalenv()) {
  if (is.character(expr))
    return(.intarith_string(expr, flags, env))

  flags <- c(flags, list(cat = FALSE))
  t <- rolog::once(call("interval", expr, expression(X), flags), env = env)
  return(t$X)
}
