#' @title
#' Constructor for intervals
#'
#' @description
#' Constructor function for objects of class interval.
#'
#' @param l
#' The lower bound of the interval.
#'
#' @param u
#' The upper bound of the interval.
#'
#' @return
#' An object of class interval.
#' 
#' @examples 
#' interval(1,2)
#'
#' @export
#' @md
interval <- function(l, u) {
  if (!(is.numeric(c(l, u))))
    stop("Non-numeric argument passed to constructor")
  if (l > u)
    stop("Upper bound is required to be greater than the lower bound")
  structure(list(l = l, u = u), class = "interval")
}
