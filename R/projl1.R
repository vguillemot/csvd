#' Compute the projection of a vector onto the l1 ball.
#'
#' @param v A vector of numerics
#' @param a The radius (>0) of the l1 ball
#' @return The projection of \eqn{v} onto the l1 ball of radius \eqn{a}.
#' @examples 
#' projl1(1:10, 21)
#' @export
projl1 <- function(v, a) {
  if (norm1(v) <= a) return(v)
  u <- sort(abs(v), decreasing = TRUE)
  n <- length(v)
  ukmaok <- (cumsum(u)-a)/(1:n)
  K <- max(which(ukmaok < u))
  tau <- ukmaok[K]
  # print(K)
  return(sign(v)*pmax(abs(v)-tau, 0))
}
