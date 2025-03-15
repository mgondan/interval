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
pnorm0 <- function(...){
  pnorm(..., lower.tail=TRUE)
}

pnorm1 <- function(...){
  pnorm(..., lower.tail=FALSE)
}

qnorm0 <- qnorm
dnorm0 <- dnorm1 <- dnorm2 <- dnorm

pt0 <- function(...){
  pt(..., lower.tail=TRUE)
}

pt1 <- function(...){
  pt(..., lower.tail=FALSE)
}

qt0 <- qt
dt0 <- dt1 <- dt


# chisquare test with/without corretion
pchisq0 <- function(...) {
  pchisq(..., lower.tail=TRUE)
}

pchisq1 <- function(...) {
  pchisq(..., lower.tail=FALSE)
}

qchisq0 <- function(...) {
  qchisq(..., lower.tail=TRUE)
}

qchisq1 <- function(...) {
  qchisq(..., lower.tail=FALSE)
}

dchisq0 <- dchisq1 <-dchisq

var_pool <- function(v_A, n_A, v_B, n_B)
{
  ((n_A - 1) * v_A + (n_B - 1) * v_B) / (n_A + n_B - 2)
}