.onAttach <- function(libname, pkgname) {
  if(!rolog::rolog_ok())
    stop("Could not attach R package rolog.")
    
  module <- system.file(file.path("prolog", "rint.pl"), package="interval")
  rolog::consult(module)
}

#' Interval
#'
#' @md
#'
#' @param expr
#' An arithmetic expression.
#'
#' @return
#' The numeric result of the expression.
#' @export
interval <- function(expr, flags = NULL, env = globalenv()) {
  expr <- rolog::once(call("term_string", expression(T), expr))
  flags <- c(flags, list(cat = FALSE))
  t <- rolog::once(call("interval", expr$T, expression(X), flags), env = env)
  return(get_result(t))
}

# Format the result from Prolog
get_result <- function(t) {
  if(is.list(t) && t$X[[1]] == "...") {
    return(paste0(t$X[[2]], "...", t$X[[3]]))
  }
  if(is.list(t) && t$X[[1]] == "...") {
    return(paste(t$X, collapse = ""))
  }
  return(t)
}
