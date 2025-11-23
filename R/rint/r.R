# This file contains functions loaded into R for the
# Prolog package rint. It is not used by the R package. 

var_pool <- function(v_A, n_A, v_B, n_B) {
  ((n_A - 1) * v_A + (n_B - 1) * v_B) / (n_A + n_B - 2)
}
