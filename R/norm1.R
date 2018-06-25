#' Compute the l1 norm of a vector.
#'
#' @param u A vector of numerics
#' @return The \eqn{l_1} norm of \eqn{u}: \eqn{\sum_i |u_i|}.
#' @examples 
#' norm1( c(0, 0, -0.5, 0.5) )
#' @export
norm1 <- function(u) sum(abs(u))
