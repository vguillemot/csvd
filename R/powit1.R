powit1 <- function(X, U0, V0, au, av, eps.pi, itermax.pi) {
  uold <- unew <- U0
  vold <- vnew <- V0
  for (iter in 1:itermax.pi) {
    vnew <- proj12(t(X) %*% uold, a=av)$x
    unew <- proj12(X %*% vnew, a=au)$x
    if ( norm2(vnew - vold) < eps.pi && norm2(unew - uold) < eps.pi ) break
    vold <- vnew
    uold <- unew
  }

  return(list(U=unew, V=vnew, iter=iter))
}