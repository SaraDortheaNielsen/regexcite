#' formulae
#'
#' @noRd
#'

read.formulae <- function(formula) {

  form <- eval.parent(formula, 2)
  ## get the environment of the formula. If this does not have an
  ## environment (it could be a character), then use the calling environment.
  form.envir <-
    if (!is.null(env <- environment(form))) {
      env
    } else {
      parent.frame(2)
    }
  ## ensure 'formula' is a formula-object:
  form <- tryCatch(formula(if (is.character(form)) form else deparse(form),
    env = form.envir
  ), error = identity)
  ## report error if the formula cannot be interpreted
  if (inherits(form, "error")) {
    stop("unable to interpret 'formula'")
  }
  environment(form) <- form.envir
  list(formula = form)
}
