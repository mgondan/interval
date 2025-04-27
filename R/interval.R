.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("rolog", quiet = TRUE))
    stop("Could not load R package rolog.")

  if (!rolog::rolog_ok())
    stop("Could not attach R package rolog.")
  module <- get_path("interval.pl")
  rolog::consult(module)
}

# Avoids code duplication by allowing the prolog files to be inside /prolog and not /inst 
# during development.
# At build time, the configure file is used to automatically create an /inst folder
get_path <- function(file) {
  if (system.file(package = "interval") == "") {
    file.path("prolog", file)
  } else {
    system.file("prolog", file, package = "interval")
  }
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
  if (t$X[[1]] == "...") {
    r <- paste0(t$X[[2]], "...", t$X[[3]])
  } else {
    r <- paste(t$X, collapse = "")
  }
  return(r)
}