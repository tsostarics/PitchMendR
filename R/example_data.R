#' Load example data path
#'
#' @param path Path to search for extdata in
#'
#' @return Path to `demo_data.csv`
#' @export
get_example_f0_data <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "PulseTransformR"))
  } else {
    system.file("extdata", path, package = "PulseTransformR", mustWork = TRUE)
  }
}
