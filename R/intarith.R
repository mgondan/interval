#' @title 
#' Evaluate expression
#'
#' @description
#' Evaluate an expression in interval arithmetic without having to create
#' interval objects.
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
#' intarith("1...2 + 3...4 + 5")
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

# For easy testing, we allow for string expressions
.intarith_string <- function(expr, flags = NULL, env = globalenv()) {
  expr <- .parse_input(expr)
  flags <- c(flags, list(cat = FALSE))
  t <- rolog::once(call("interval", expr, expression(X), flags), env = env)
  return(.get_result(t))
}

.parse_input <- function(x) {
  expr <- rolog::once(call("term_string", expression(T), x))
  if (!is.list(expr)) {
    stop(paste("Could not parse input string:", x), call. = FALSE)
  }
  return(expr$T)
}

# Format the result from Prolog
.get_result <- function(t) {
  if (is.list(t) && t$X[[1]] == "...") {
    return(interval(t$X[[2]], t$X[[3]]))
  }
  if (is.list(t)) {
    return(t$X)
  }
  return(t)
}
