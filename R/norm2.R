#' Compute the l2 norm of a vector.
#'
#' @param u A vector of numerics
#' @return The l2 norm of \eqn{u}: \eqn{\sqrt{\sum_i u_i^2}}.
#' @examples 
#' norm2( c(0, 0, -0.5, 0.5) )
#' @export
norm2 <- function(u) sqrt(sum(u**2))
