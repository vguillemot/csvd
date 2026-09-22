#' Compute the projection of a vector onto the l1 ball.
#'
#' @name projection
#' @rdname projection
#' @param v A vector of numerics
#' @param a The radius (>0) of the l1 ball
#' @return The projection of \eqn{v} onto the l1 ball of radius \eqn{a}.
#' @examples
#' projl1(1:10, 21)
#' @export
projl1 <- function(v, a) {
  if (norm1(v) <= a) {
    return(v)
  }
  u <- sort(abs(v), decreasing = TRUE)
  n <- length(v)
  thresholds <- (cumsum(u) - a) / seq_len(n)
  index <- max(which(thresholds < u))
  threshold <- thresholds[index]
  sign(v) * pmax(abs(v) - threshold, 0)
}

#' Compute the projection onto the intersection of the l1 and l2 balls.
#'
#' @rdname projection
#' @param x A vector of numerics
#' @return The projection of \eqn{x} onto \eqn{B_1(a) \cap B_2}.
#' @examples
#' proj12(1:10, 21)
#' @importFrom stats runif
#' @export
proj12 <- function(x, a = 1) {
  norm2_x <- norm2(x)
  if (norm2_x < 1e-32) {
    return(list(x = x))
  }
  if (sum(abs(x / norm2_x)) <= a) {
    return(list(x = x / norm2_x, k = NaN))
  }
  nonzero <- x != 0
  p <- abs(x[nonzero])
  max_value <- max(p)
  at_max <- p == max_value
  max_count <- sum(at_max)
  if (a < sqrt(max_count)) {
    stop("Impossible to project, minimum ratio is: ", sqrt(max_count))
  }
  if (a == sqrt(max_count)) {
    x_soft <- rep(0, length(x))
    x_soft[nonzero][at_max] <- 1 / sqrt(max_count)
    return(list(x = x_soft, k = NaN))
  }

  sum_1 <- sum_2 <- count <- 0
  for (iteration in seq_len(5000)) {
    sample_size <- length(p)
    if (sample_size == 0) {
      warning("length(p) = 0")
      break
    }
    candidate <- p[round(runif(1, 1, sample_size), 0)]
    while (candidate == max_value) {
      candidate <- p[round(runif(1, 1, sample_size), 0)]
    }
    lower <- p[p < candidate]
    upper <- p[p > candidate]
    equal_count <- sum(p == candidate)
    rank <- count + length(upper) + equal_count
    candidate_squared <- candidate^2
    lower_sum <- sum(upper) + equal_count * candidate
    lower_squared <- sum(upper^2) + equal_count * candidate_squared
    psi <- (sum_1 + lower_sum - rank * candidate) /
      sqrt(sum_2 + lower_squared - 2 * candidate *
        (sum_1 + lower_sum) + rank * candidate_squared)
    if (psi > a) {
      if (length(upper) == 0) {
        break
      }
      p <- upper
    } else {
      if (length(lower) == 0) {
        break
      }
      next_candidate <- max(lower)
      next_psi <- (sum_1 + lower_sum - rank * next_candidate) /
        sqrt(sum_2 + lower_squared - 2 * next_candidate *
          (sum_1 + lower_sum) + rank * next_candidate^2)
      if (next_psi > a) {
        break
      }
      p <- lower
      count <- rank
      sum_1 <- sum_1 + lower_sum
      sum_2 <- sum_2 + lower_squared
    }
  }

  lambda <- candidate -
    (a * sqrt((rank - psi^2) / (rank - a^2)) - psi) *
    (sum_1 + lower_sum - rank * candidate) / (psi * rank)
  x_soft <- sign(x) * pmax(0, abs(x) - lambda)
  list(x = x_soft / norm2(x_soft), k = NaN)
}

#' Compute the projection onto the intersection of l1, l2 and orthogonal space.
#'
#' @rdname projection
#' @param M A matrix of vectors
#' @param itermax The maximum number of iterations
#' @param eps Precision
#' @return The projected vector and number of iterations.
#' @examples
#' proj12orth(1:10, a = 10, M = normalize(rnorm(10)))
#' @export
proj12orth <- function(x, a = 1, M, itermax = 5000, eps = 1e-16) {
  x_old <- x_new <- x
  for (iteration in seq_len(itermax)) {
    if (is.null(M)) {
      x_new <- proj12(x_old, a = a)$x
    } else {
      x_new <- proj12(projorth(x_old, M), a = a)$x
    }
    if (norm2(x_new - x_old) < eps) {
      break
    }
    x_old <- x_new
  }
  list(x = x_new, k = iteration)
}

#' Compute the projection onto a group l1 ball.
#'
#' @rdname projection
#' @param g A factor describing the groups
#' @return The projection of \eqn{v} onto the group l1 ball of radius \eqn{a}.
#' @examples
#' projgroup(1:10, rep(1:2, each = 5), 21)
#' @export
projgroup <- function(v, g, a) {
  if (!is.numeric(v)) {
    stop("v must be numeric")
  }
  if (length(v) != length(g)) {
    stop("v and g must have the same length")
  }
  if (length(a) != 1 || !is.numeric(a) || is.na(a) || a < 0) {
    stop("a must be a non-negative numeric scalar")
  }
  if (anyNA(v) || anyNA(g)) {
    stop("v and g must not contain missing values")
  }
  if (normgroup(v, g) <= a) {
    return(v)
  }

  group_indices <- split(seq_along(v), g, drop = TRUE)
  group_norms <- vapply(
    group_indices,
    function(indices) norm2(v[indices]),
    numeric(1)
  )
  sorted_norms <- sort(group_norms, decreasing = TRUE)
  cumulative_norms <- cumsum(sorted_norms)
  thresholds <- (cumulative_norms - a) / seq_along(sorted_norms)
  active_groups <- which(sorted_norms > thresholds)
  threshold <- thresholds[max(active_groups)]

  result <- v
  shrinkage <- pmax(0, 1 - threshold / group_norms)
  for (group_number in seq_along(group_indices)) {
    result[group_indices[[group_number]]] <-
      v[group_indices[[group_number]]] * shrinkage[group_number]
  }
  result
}

#' Compute the projection onto the space orthogonal to a matrix.
#'
#' @rdname projection
#' @return The projection of \eqn{x} onto \eqn{V^\perp}.
#' @examples
#' projorth(1:10, normalize(rnorm(10)))
#' @export
projorth <- function(x, M) {
  M_transpose_x <- t(M) %*% x
  M_M_transpose_x <- M %*% M_transpose_x
  x - M_M_transpose_x
}