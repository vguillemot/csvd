powit2 <- function(X, U0, V0, Uorth, Vorth, au, av, eps.pi, eps.pocs, itermax.pi, itermax.pocs) {
  uold <- unew <- U0
  vold <- vnew <- V0
  for (iter in 1:itermax.pi) {
    vnew <- proj12orth(t(X) %*% uold, a=av, M=Vorth, 
                       itermax = itermax.pocs, eps = eps.pocs)$x
    unew <- proj12orth(X %*% vnew, a=au, M=Uorth, 
                       itermax = itermax.pocs, eps = eps.pocs)$x
    if ( norm2(vnew - vold) < eps.pi && norm2(unew - uold) < eps.pi ) break
    vold <- vnew
    uold <- unew
  }
  
  return(list(U=unew, V=vnew, iter=iter))
}