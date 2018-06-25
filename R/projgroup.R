#' Compute the projection of a vector onto the l1 ball.
#'
#' @param v A vector of numerics
#' @param a The radius (>0) of the l1 ball
#' @param g A factor describing the groups
#' @return The projection of \eqn{v} onto the l1 ball of radius \eqn{a}.
#' @examples 
#' projl1(1:10, 21)
#' @export
projgroup <- function(v, g, a) {
  if (normgroup(v, g) <= a) return(v)
  # vecnormg <- tapply(v, g, FUN = norm2)
  vnormg <- ave(v, g, FUN=norm2)
  vv <- ifelse(vnormg==0, 0, vv / vnormg)
  nu <- sort(abs(tapply))
  ifelse(vecnormg == 0, )
  v <- sort(abs())
  
  
  u <- sort(abs(v), decreasing = TRUE)
  n <- length(v)
  ukmaok <- (cumsum(u)-a)/(1:n)
  K <- max(which(ukmaok < u))
  tau <- ukmaok[K]
  # print(K)
  return(sign(v)*pmax(abs(v)-tau, 0))
}

proxl1l2 <- function(u, g, a) {
  zenormg <- ave(u, g, FUN=norm2)
  boolu <- (zenormg >= a) + 0
  shrinku <- (1 - a/zenormg) * u
  return(boolu * shrinku)
}

