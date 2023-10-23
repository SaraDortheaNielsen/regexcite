#' frames
#'
#' @noRd
#'

make.modelmatrix <- function(modelcall,formulae,formulae_random_a,formulae_random_b) {
  ## Extract full model.frame (fullmf):
  m <- match("data",names(modelcall), 0)
  mf <- modelcall[c(1, m)]
  mf$formula <- formulae$formula
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  fullmf <- eval(mf, envir = parent.frame(2))

  mf <- modelcall[c(1, m)]
  mf$formula <- formulae_random_a$formula
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  random_a_mf <- eval(mf, envir = parent.frame(2))

  mf <- modelcall[c(1, m)]
  mf$formula <- formulae_random_b$formula
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  random_b_mf <- eval(mf, envir = parent.frame(2))

  y  = getY(fullmf)
  X  = getX(fullmf)
  ZA = getZ(random_a_mf,"a")
  ZB = getZ(random_b_mf,"b")

  modelmat = getModelMatrix(y,X)

  ## return:
  list(mf = fullmf,
       y  = y,
       X  = X,
       ZA = ZA,
       ZB = ZB,
       B1 = modelmat$B1,
       B2 = modelmat$B2,
       o1 = modelmat$o1,
       o2 = modelmat$o2
  )
}
