#' main
#'
#' @param model formula for model
#' @param random_a Random effect crossed with ub
#' @param random_b Random effect crossed with ua
#' @param data dataframe holding data
#'
#' @return list
#' @export
#'

main <- function(model,random_a,random_b,data) {
  # Setting up model matrices
  # The code to set up the model matrices is a modified version of code from the ordinal package in R
  # Thank you to Rune Haubo Christensen who wrote the code for the ordinal package
  mc <- match.call(expand.dots = FALSE)

  if (missing(model)) stop("A model is needed")
  if (missing(data)) stop("Data is required")
  if (missing(random_a)) stop("The crossed random factor 'a' is missing")
  if (missing(random_b)) stop("The crossed random factor 'b' is missing")

  formulae <- read.formulae(formula = model)
  check.data(data = data)
  check.random(random_a,"a")
  check.random(random_b,"b")
  formulae_random_a <- read.formulae(formula = formula(paste0("~ -1 +",random_a)))
  formulae_random_b <- read.formulae(formula = formula(paste0("~ -1 +",random_b)))


  frames <- make.modelmatrix(modelcall          = mc,
                             formulae           = formulae,
                             formulae_random_a  = formulae_random_a,
                             formulae_random_b  = formulae_random_b)

  return(list(y = frames$y, B1 = frames$B1, o1 = frames$o1))
}
