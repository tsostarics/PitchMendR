#' Get pitch candidate an octave away from current F0
#'
#' @description
#' Given a frame containing pitch candidate information and a desired octave
#' transformation (either "halve" or "double), look up which candidate (if any)
#' corresponds to that transformation. E.g., if F0 is 110 and we want to double,
#' this will look up which candidate is equal to 220. Because getting values at
#' ratios of exactly 2 or 1/2 is unlikely, a small threshold is added. So, this
#' will actually look for a candidate between `[220-threshold, 220+threshold]`
#'
#' This shouldn't be called by itself, only within get_and_set_freq_values in
#' module_sauce_octaveshift.R
#'
#' @param frame List from pitch object. E.g., `pitchObj$frame[[i]]`
#' @param octave String, either "halve" or "double"
#' @param threshold Threshold value in semitones, default .25 (25 cents) but subject to tuning
#'
#' @returns integer index of the candidate an octave away. If no such
#' candidate is found, returns 1 (index of the current f0 value).
get_value_octave_away <- function(frame, octave, threshold = 0.25) {
  # This is done in get_and_set_freq_values
  # octave <- rlang::arg_match0(octave, c("halve", "double"))
  ratio <- switch (octave,
                   "halve" = 0.5,
                   "double" = 2)
  cur_f0 <- frame[["frequency"]][1]

  fmax <- add_semitones(cur_f0 * ratio, threshold)
  fmin <- add_semitones(cur_f0 * ratio, -threshold)

  new_frame_i <-
    which(frame[["frequency"]] <= fmax &
            frame[["frequency"]] >= fmin)[1L]

  if (is.na(new_frame_i))
    new_frame_i <- 1L

  new_frame_i
}
