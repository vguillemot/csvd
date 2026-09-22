#' Compute one constrained power iteration.
#'
#' @name power
#' @rdname power
#' @param X A matrix
#' @param U0 Initial left singular vector
#' @param V0 Initial right singular vector
#' @param au Left-vector radius
#' @param av Right-vector radius
#' @param eps.pi Power iteration precision
#' @param itermax.pi Maximum number of power iterations
#' @return A list containing the updated vectors and iteration count.
powit1 <- function(X, U0, V0, au, av, eps.pi, itermax.pi) {
  u_old <- u_new <- U0
  v_old <- v_new <- V0
  for (iteration in seq_len(itermax.pi)) {
    v_new <- proj12(t(X) %*% u_old, a = av)$x
    u_new <- proj12(X %*% v_new, a = au)$x
    if (norm2(v_new - v_old) < eps.pi &&
        norm2(u_new - u_old) < eps.pi) {
      break
    }
    v_old <- v_new
    u_old <- u_new
  }
  list(U = u_new, V = v_new, iter = iteration)
}

#' Compute one orthogonally constrained power iteration.
#'
#' @rdname power
#' @param Uorth Existing left vectors
#' @param Vorth Existing right vectors
#' @param eps.pocs Projection precision
#' @param itermax.pocs Maximum number of projection iterations
powit2 <- function(
    X, U0, V0, Uorth, Vorth, au, av, eps.pi, eps.pocs,
    itermax.pi, itermax.pocs) {
  u_old <- u_new <- U0
  v_old <- v_new <- V0
  for (iteration in seq_len(itermax.pi)) {
    v_new <- proj12orth(
      t(X) %*% u_old, a = av, M = Vorth,
      itermax = itermax.pocs, eps = eps.pocs
    )$x
    u_new <- proj12orth(
      X %*% v_new, a = au, M = Uorth,
      itermax = itermax.pocs, eps = eps.pocs
    )$x
    if (norm2(v_new - v_old) < eps.pi &&
        norm2(u_new - u_old) < eps.pi) {
      break
    }
    v_old <- v_new
    u_old <- u_new
  }
  list(U = u_new, V = v_new, iter = iteration)
}