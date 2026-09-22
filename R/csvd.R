#' Constrained SVD of a matrix (wrapper of c++ functions).
#'
#' @param X a (data) matrix;
#' @param R the desired rank of the singular decomposition;
#' @param au The radiuses (>0) of the
#' $L_1$ ball for each left vector
#' @param av The radiuses
#' (>0) of the $L_1$ balls for each right vector
#' @param itermax.pi The maximum number of power iterations
#' @param itermax.pocs The maximum number of projection iterations
#' @param eps.pi The power iteration precision
#' @param eps.pocs The projection precision
#' @param init How to initialize the algorithm
#' @return Pseudo-singular vectors and values
#' @examples
#' X <- matrix(rnorm(20), 5, 4)
#' csvd(X)
#' @author Vincent Guillemot
#' @importFrom MASS mvrnorm
#' @export
csvd <- function(X, R=2, au = rep(1.4, R), av = rep(1.4, R), 
                 itermax.pi=1000, itermax.pocs=1000,
                 eps.pi=1e-16, eps.pocs=1e-16, init="svd") {
  check_inputs(
    X, R, au, av, itermax.pi, itermax.pocs, eps.pi, eps.pocs, init
  )
  I <- nrow(X)
  J <- ncol(X)
  
  if (I==1 & J==1) stop("Are you sure you want to perform the SVD of a scalar?")
  # if (R<=1) stop("R should be > 1")

  nas <- is.na(X)
  if (any(nas)) X[nas] <- mean(X, na.rm = TRUE)
  
  # print(sprintf("number of NAs:%i", sum(nas)))
  
  # Build initialization matrices either with SVD (prefered method) or randomly
  if (init=="svd") {
    svdx <- svd(X, nu=R, nv=R)
    U0 <- svdx$u
    V0 <- svdx$v
  } else if( init=="rand") {
    U0 <- 1/(I-1) * mvrnorm(n = I, mu = rep(0,R), 
                            Sigma = diag(R), empirical = TRUE)
    V0 <- 1/(J-1) * mvrnorm(n = J, mu = rep(0,R), 
                            Sigma = diag(R), empirical = TRUE)
  } else {
    stop("init should be either svd or rand.")
  }
  U <- matrix(0, I, R)
  V <- matrix(0, J, R)
  
  iter <- rep(NA, R)
  
  ## Power iteration without orth projection
  res.powit1 <- powit1(X, 
                       U0[,1,drop=FALSE], V0[,1,drop=FALSE], 
                       au[1], av[1], 
                       eps.pi, 
                       itermax.pi)
  U[,1] <- res.powit1$U
  V[,1] <- res.powit1$V
  iter[1] <- res.powit1$iter
  
  if (R > 1) {
    for (r in 2:R) {
      ## Power Iteration with orth projection
      res.powit2 <- powit2(X,                        # original matrix
                           U0[,r], V0[,r],           # initialization vectors
                           U[,1:(r-1),drop=FALSE], V[,1:(r-1),drop=FALSE], # orth constraint
                           au[r], av[r],             # sparsity constrain
                           eps.pi, eps.pocs,         # precision
                           itermax.pi, itermax.pocs) # max iteration
      
      U[,r] <- res.powit2$U
      V[,r] <- res.powit2$V
      iter[r] <- res.powit2$iter
      
    }
  }
  
  D <- diag(t(U) %*% X %*% V)
  oD <- order(D, decreasing = TRUE)
  # oD <- 1:R
  res <- list(U=U[,oD], V=V[,oD], D=D[oD], iter=iter[oD])
  return(res)
}

#' Validate the inputs of `csvd()`.
#'
#' @param X A numeric matrix.
#' @param R The requested rank.
#' @param au Left-vector radii.
#' @param av Right-vector radii.
#' @param itermax.pi Maximum number of power iterations.
#' @param itermax.pocs Maximum number of projection iterations.
#' @param eps.pi Power iteration precision.
#' @param eps.pocs Projection precision.
#' @param init Initialization method.
#' @return Invisibly returns `TRUE` when all inputs are valid.
#' @noRd
check_inputs <- function(
    X, R, au, av, itermax.pi, itermax.pocs, eps.pi, eps.pocs, init) {
  if (!is.matrix(X) || !is.numeric(X)) {
    stop("X must be a numeric matrix")
  }
  if (nrow(X) < 2 || ncol(X) < 2) {
    stop("X must have at least two rows and two columns")
  }
  if (all(is.na(X))) {
    stop("X must contain at least one non-missing value")
  }
  if (any(is.infinite(X))) {
    stop("X must not contain infinite values")
  }
  if (length(R) != 1 || !is.numeric(R) || !is.finite(R) ||
      R < 1 || R != as.integer(R) || R > min(dim(X))) {
    stop("R must be an integer between 1 and min(dim(X))")
  }
  if (length(au) != R || !is.numeric(au) || any(!is.finite(au)) ||
      any(au <= 0)) {
    stop("au must contain R positive finite values")
  }
  if (length(av) != R || !is.numeric(av) || any(!is.finite(av)) ||
      any(av <= 0)) {
    stop("av must contain R positive finite values")
  }
  if (length(itermax.pi) != 1 || !is.numeric(itermax.pi) ||
      !is.finite(itermax.pi) || itermax.pi < 1 ||
      itermax.pi != as.integer(itermax.pi)) {
    stop("itermax.pi must be a positive integer")
  }
  if (length(itermax.pocs) != 1 || !is.numeric(itermax.pocs) ||
      !is.finite(itermax.pocs) || itermax.pocs < 1 ||
      itermax.pocs != as.integer(itermax.pocs)) {
    stop("itermax.pocs must be a positive integer")
  }
  if (length(eps.pi) != 1 || !is.numeric(eps.pi) ||
      !is.finite(eps.pi) || eps.pi <= 0) {
    stop("eps.pi must be a positive finite value")
  }
  if (length(eps.pocs) != 1 || !is.numeric(eps.pocs) ||
      !is.finite(eps.pocs) || eps.pocs <= 0) {
    stop("eps.pocs must be a positive finite value")
  }
  if (length(init) != 1 || !is.character(init) ||
      !init %in% c("svd", "rand")) {
    stop("init must be either 'svd' or 'rand'")
  }
  invisible(TRUE)
}