#' Zoom by
#'
#' Adapted from https://github.com/praat/praat/blob/8ef0fca519d833d55729a339bd73226074215fe7/foned/FunctionEditor.cpp#L471
#' so the zooming behavior is the same as that in Praat.
#'
#' @param current_xmin Current xmin of the plot
#' @param current_xmax Current xmax of the eplot
#' @param factor Factor to zoom in by, default 1 (=no zoom), but
#' will typically be either 0.5 (zoom in) or 2.0 (zoom out)
#'
#' @return Numeric vector of length 2 with the new x range values to use
zoom_by <- function(current_xmin, current_xmax, factor = 1) {
  current_size <- current_xmax - current_xmin
  new_size <- current_size * factor
  increase <- new_size - current_size
  shift <- 0.5 * increase

  # Return the new xrange
  c(current_xmin - shift, current_xmax + shift)
}
