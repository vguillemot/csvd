#' Compute the sum of squares.
#'
#' @param x A vector of numerics
#' @return The sum of the squared coefficients of vector x.
#' @examples 
#' ssq(1:10)
#' @export
ssq <- function(u) sum(u**2)
