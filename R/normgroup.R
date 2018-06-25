#' Compute the l1,2 group norm of a vector.
#'
#' @param u A vector of numerics
#' @param g A group factor
#' @return The \eqn{l_{1,2}} group norm of \eqn{u}: \eqn{\sum_g ||u_g||_2}.
#' @examples 
#' normgroup( c(0, 0, -0.5, 0.5), gl(2, 2) )
#' @export
normgroup <- function(u, g) {
  sum(tapply(X = u, INDEX = g, FUN = norm2))
}
