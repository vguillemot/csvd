#' Compute the group l1,2 proximal operator.
#'
#' @name prox
#' @rdname prox
#' @param u A vector of numerics
#' @param g A factor describing the groups
#' @param a The proximal threshold
#' @return The group soft-thresholded vector.
#' @importFrom stats ave
proxl1l2 <- function(u, g, a) {
  group_norms <- ave(u, g, FUN = norm2)
  shrinkage <- ifelse(
    group_norms == 0,
    0,
    pmax(0, 1 - a / group_norms)
  )
  u * shrinkage
}