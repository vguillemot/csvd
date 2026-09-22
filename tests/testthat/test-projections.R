test_that("group projection reaches the requested radius", {
  values <- c(3, 4, 1, 0)
  groups <- c(1, 1, 2, 2)
  projected <- projgroup(values, groups, 4)

  expect_equal(normgroup(projected, groups), 4, tolerance = 1e-12)
  expect_equal(projected[1:2] / values[1:2], c(0.8, 0.8))
  expect_equal(projected[3:4], c(0, 0))
})

test_that("group projection leaves vectors inside the ball unchanged", {
  values <- c(1, 2, 0, 0)
  groups <- c(1, 1, 2, 2)

  expect_identical(projgroup(values, groups, 4), values)
})

test_that("group projection validates its inputs", {
  expect_error(projgroup(1:3, c(1, 1), 1), "same length")
  expect_error(projgroup(c(1, NA), c(1, 1), 1), "missing")
  expect_error(projgroup(1:2, c(1, 2), -1), "non-negative")
})

test_that("group proximal operator handles zero groups", {
  values <- c(3, 4, 0, 0)
  groups <- c(1, 1, 2, 2)

  expect_equal(
    csvd:::proxl1l2(values, groups, 1),
    c(2.4, 3.2, 0, 0)
  )
})

test_that("csvd validates its main inputs", {
  matrix_values <- matrix(1:6, nrow = 3)

  expect_error(csvd(1:6), "numeric matrix")
  expect_error(csvd(matrix_values, R = 4), "between 1 and min")
  expect_error(csvd(matrix(NA_real_, nrow = 3, ncol = 2)), "non-missing")
  expect_error(csvd(matrix_values, au = c(1, 2, 3)), "contain R")
  expect_error(csvd(matrix_values, itermax.pi = 0), "positive integer")
  expect_error(csvd(matrix_values, init = "random"), "either 'svd' or 'rand'")
})

test_that("csvd handles partial missing values and aligns iterations", {
  set.seed(1)
  matrix_values <- matrix(rnorm(20), nrow = 5)
  matrix_values[1, 1] <- NA_real_

  result <- csvd(
    matrix_values,
    itermax.pi = 5,
    itermax.pocs = 5,
    eps.pi = 1e-8,
    eps.pocs = 1e-8
  )

  expect_length(result$D, 2)
  expect_length(result$iter, 2)
  expect_true(all(is.finite(result$D)))
  expect_true(all(is.finite(result$iter)))
})