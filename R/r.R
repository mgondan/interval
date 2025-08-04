`...` <- function(lo, hi)
  call("...", lo, hi)

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

var_pool <- function(v_A, n_A, v_B, n_B)
{
  ((n_A - 1) * v_A + (n_B - 1) * v_B) / (n_A + n_B - 2)
}
