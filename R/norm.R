#' Compute the l1 norm of a vector.
#'
#' @name norm
#' @rdname norm
#' @param u A vector of numerics
#' @return The l1 norm of \eqn{u}: \eqn{\sum_i |u_i|}.
#' @examples
#' norm1(c(0, 0, -0.5, 0.5))
#' @export
norm1 <- function(u) sum(abs(u))

#' Compute the l2 norm of a vector.
#'
#' @rdname norm
#' @return The l2 norm of \eqn{u}: \eqn{\sqrt{\sum_i u_i^2}}.
#' @examples
#' norm2(c(0, 0, -0.5, 0.5))
#' @export
norm2 <- function(u) sqrt(sum(u^2))

#' Compute the l1,2 group norm of a vector.
#'
#' @rdname norm
#' @param g A group factor
#' @return The \eqn{l_{1,2}} group norm of \eqn{u}: \eqn{\sum_g ||u_g||_2}.
#' @examples
#' normgroup(c(0, 0, -0.5, 0.5), gl(2, 2))
#' @export
normgroup <- function(u, g) {
  sum(tapply(X = u, INDEX = g, FUN = norm2))
}

#' Normalize a vector.
#'
#' @rdname norm
#' @return The normalized version of \eqn{u}.
#' @examples
#' normalize(c(0, 0, -0.5, 0.5))
#' @export
normalize <- function(u) {
  norm_u <- norm2(u)
  if (norm_u == 0) {
    return(u)
  }
  u / norm_u
}