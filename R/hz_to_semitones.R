#' Calculate semitone differences
#'
#' Calculates a vector of Hz values to semitone differences from a baseline.
#' Resulting vector includes a `semitones_from` attribute.
#' Access with `attr(semitone_vector, 'semitones_from')`.
#'
#' @param hz_vals Numeric vector of Hz values
#' @param .semitones_from Value to calculate semitones from. If left as NA,
#' a baseline will be created by using `.startmethod` with the first
#' `.n_starting_samples` pitch points
#' @param .n_starting_samples Integer number of starting pitch points to use
#' for calculating the baseline
#' @param .startmethod Function to create the baseline. Default is `median` with
#' the first `.n_starting_samples`. Other recommendations are `mean` or `first`.
#' @param .quiet Whether to message user about value selected for baseline
#'
#' @return Vector of semitone values
#' @export
#' @examples
#'
#' hz_to_semitones(c(110, 220, 440, 540), .semitones_from = 110)
#'
#' @importFrom utils head
hz_to_semitones <- function(hz_vals,
                            .semitones_from = NA,
                            .n_starting_samples = 5L,
                            .startmethod = stats::median,
                            .quiet = FALSE) {
  if (is.na(.semitones_from)){
    .semitones_from <- .startmethod(head(hz_vals, .n_starting_samples))
    if (!.quiet)
      message(paste0("Calculating semitones from ", round(.semitones_from, 2)))
  }

  semitones <- vapply(hz_vals,
                      function(hz)
                        12 * log(hz / .semitones_from, base=2),
                      1.0)
  attr(semitones, "semitones_from") <- .semitones_from
  semitones
}
