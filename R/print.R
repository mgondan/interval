#' @title
#' Print intervals
#' 
#' @description
#' Print intervals as "lower...upper".
#'
#' @param x
#' An object of class interval.
#'
#' @param ...
#' Further arguments.
#'
#' @export
#' @md
print.interval <- function(x, ...) {
  cat(x$l, "...", x$u, "\n", sep = "")
}

