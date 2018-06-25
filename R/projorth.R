#' Compute the projection of a vector onto the space orthogonal to a given 
#' matrix.
#'
#' @param x A vector of numerics
#' @param M A matrix of vectors
#' @return The projection of \eqn{x} onto \eqn{V^\perp}.
#' @examples 
#' projorth(1:10, normalize(rnorm(10)))
#' @export
projorth <- function(x, M) {
  Mtx <- t(M) %*% x
  MMtx <- M %*% Mtx
  x - MMtx
}
