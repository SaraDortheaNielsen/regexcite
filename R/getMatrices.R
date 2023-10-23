#' getMatrices
#'
#' @importFrom stats model.matrix model.response
#'
#' @noRd
#'


getY <- function(mf) {
  ### Extract model response:
  y <- model.response(mf)
  if (!is.factor(y)) stop("response needs to be a factor")
  y
}

getX <- function(mf) {
  X <- model.matrix(attributes(mf)$terms, mf)
  n <- nrow(X)
  ## remove intercept from X:
  Xint <- match("(Intercept)", colnames(X), nomatch = 0)
  if (Xint <= 0) {
    X <- cbind("(Intercept)" = rep(1, n), X)
    stop("an intercept is needed and assumed")
  } ## intercept in X is guaranteed.
  X
}

getZ <- function(mf,indicator) {

  if (attr(terms(mf),"dataClasses") != "factor") stop(paste0("Random effect ",indicator," has to be a factor"))
  if (length(attr(terms(mf),"term.labels"))>1) stop(paste0("Only one effect is allowed for random effect ",indicator))
  if (attr(terms(mf),"intercept") != 0) stop(paste0("do not indicate an intercept for random effect ",indicator))
  Z <- model.matrix(attributes(mf)$terms, mf)
  Z
}

getModelMatrix <- function(y, X) {
  ## Make B1, B2, o1, o2 based on y, X:
  y <- droplevels(y)
  ntheta <- nlevels(y) - 1
  y <- c(unclass(y))
  y[is.na(y)] <- 0
  n <- length(y)
  TotCut <- 1 * (col(matrix(0, nrow(X), ntheta + 1)) == y)
  o1 <- c(1e4 * TotCut[, ntheta + 1])
  o2 <- c(-1e4 * TotCut[, 1])
  A1 <- TotCut[, -(ntheta + 1), drop = FALSE]
  A2 <- TotCut[, -1, drop = FALSE]

  ## update B1 and B2 with location effects (X):
  nbeta <- NCOL(X) - 1
  if (nbeta > 0) {
    B1 <- cbind(A1, -X[, -1, drop = FALSE])
    B2 <- cbind(A2, -X[, -1, drop = FALSE])
  } else {
    B1 <- A1
    B2 <- A2
  }
  dimnames(B1) <- NULL
  dimnames(B2) <- NULL

  lst <- list(
    A1 = A1, A2 = A2,
    B1 = B1, B2 = B2,
    o1 = o1, o2 = o2
  )
  return(lst)
}
