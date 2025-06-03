#' PitchMendR
#'
#' @importFrom Rcpp evalCpp sourceCpp
#' @useDynLib PitchMendR
#' @name PitchMendR
NULL

utils::globalVariables(c("F0_of_err",
                         "F0_semitones",
                         "carryover_err",
                         "carryover_only",
                         "err",
                         "flagged_samples",
                         ".",
                         "boundkeys",
                         "frame",
                         "frame_i",
                         "f0_i",
                         "zero_index",
                         "f0",
                         "original_f0",
                         "cand_rank",
                         "is_voiced",
                         "pulse_id",
                         'notes',
                         "keep_pulse",
                         "pulse_transform",
                         "file_checked",
                         "boundKeys",
                         "data",
                         "f0_st_diff",
                         "is_fall_error",
                         "is_rise_error",
                         "is_threshold_error",
                         "lead_F0_Hz",
                         "lead_F0_semitones",
                         "next_F0_st",
                         "ratio_Hz",
                         "time_diff"))

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "howtoimages",
      package = "PitchMendR"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}
