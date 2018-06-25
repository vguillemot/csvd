#' Compute the projection of a vector onto the intersection of the l1 ball,
#' the l2 ball and the space orthogonal to M.
#'
#' @param x A vector of numerics
#' @param a The radius (>0) of the l1 ball
#' @param M A matrix of vectors
#' @param itermax The maximum number of iterations
#' @param eps Precision
#' @return The projection of \eqn{x} onto \eqn{B_1(a) \cap B_2 \cap M^\perp}.
#' @examples 
#' proj12orth(1:10, a=10, M=normalize(rnorm(10)))
#' @export
proj12orth <- function(x, a=1, M, itermax=5000, eps=1e-16) {
  xold <- xnew <- x
  for (k in 1:itermax) {
    if (is.null(M)) {
      xnew <- proj12(xold, a=a)$x
      # xnew <- projl1(normalize(xold), a=a)
      # xnew <- 1/2 * ( projl1(xold, a=a) + normalize(xold) )
    } else {
      xnew <- proj12(projorth(xold, M), a=a)$x
      # xnew <- projl1(projorth(normalize(xold), M), a=a)
      # xnew <- 1/3 * ( projl1(xold, a=a) + normalize(xold) + projorth(xold, M) )
    }
    if ( norm2(xnew - xold) < eps ) break
    xold <- xnew
  }
  list(x=xnew, k=k)
}
