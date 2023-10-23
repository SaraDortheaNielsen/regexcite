#' data
#'
#' @noRd
#'

# Check that data is a dataframe.
check.data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be of type 'data.frame'.")
  }
}
