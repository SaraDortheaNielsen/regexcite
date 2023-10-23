#' random_effects
#'
#' @noRd
#'

# Check that data is a dataframe.
check.random <- function(random,indicator) {
  if (!is.character(random)) {
    stop(paste0("The random effect ",indicator," has to be a character string"))
  }
}
