.onAttach <- function(libname, pkgname) {
#  if(!requireNamespace("rolog", quiet=TRUE))
#    stop("Could not load R package rolog.")

  if(!rolog::rolog_ok())
    stop("Could not attach R package rolog.")

#  module <- system.file(file.path("prolog", "interval.pl"), package="interval")
#  rolog::consult(module)

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

pbinom0 <- function(...)
  pbinom(..., lower.tail=TRUE)

pbinom1 <- function(...)
  pbinom(..., lower.tail=FALSE)

qbinom0 <- function(...)
  qbinom(..., lower.tail=TRUE)

qbinom1 <- function(...)
  qbinom(..., lower.tail=FALSE)

dbinom0 <- dbinom1 <- dbinom

dbinom_max = function(k, N, p1, p2)
  dbinom(k, N, ifelse(k > N * p2, p2, ifelse(k < N * p1, p1, k/N)))

dbinom2 = function(k1, k2, N1, N2, p1, p2)
{
  d1 = dbinom(c(k1, k2), c(N2, N1), c(p2, p1))
  if(k2 < N1 * p1)
    return(d1)

  if(k1 > N2 * p2)
    return(rev(d1))
  
  g = expand.grid(k=k1:k2, N=N1:N2)
  g = g[g$k >= floor(p1 * g$N) & g$k <= ceiling(p2 * g$N), ]
  d2 = dbinom_max(g$k, g$N, p1, p2)
  c(min(d1), max(d2))
}

# pnorm/qnorm with 1 argument z
pnorm0 <- function(...)
  pnorm(..., lower.tail=TRUE)

pnorm1 <- function(...)
  pnorm(..., lower.tail=FALSE)

qnorm0 <- function(...)
  qnorm(..., lower.tail=TRUE)

qnorm1 <- function(...)
  qnorm(..., lower.tail=FALSE)

dnorm0 <- dnorm1 <- dnorm2 <- dnorm

pt0 <- function(...)
  pt(..., lower.tail=TRUE)

pt1 <- function(...)
  pt(..., lower.tail=FALSE)

qt0 <- function(...)
  qt(..., lower.tail=TRUE)

qt1 <- function(...)
  qt(..., lower.tail=FALSE)

dt0 <- dt1 <- dt

pchisq0 <- function(...)
  pchisq(..., lower.tail=TRUE)

pchisq1 <- function(...)
  pchisq(..., lower.tail=FALSE)

qchisq0 <- function(...)
  qchisq(..., lower.tail=TRUE)

qchisq1 <- function(...)
  qchisq(..., lower.tail=FALSE)

dchisq0 <- dchisq1 <-dchisq

var_pool <- function(v_A, n_A, v_B, n_B)
  ((n_A - 1) * v_A + (n_B - 1) * v_B) / (n_A + n_B - 2)

