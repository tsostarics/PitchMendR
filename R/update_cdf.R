#' Update candidate dataframe
#'
#' Updates a candidate data frame in the raw pitch database by swapping the
#' first index with the given candidate
#'
#' @param rawPitchDB rawPitchDB
#' @param plotted_file File to use to index into the database
#' @param which_frame which frame to subset to
#' @param new_candidate_i index of the new candidate, typically not 1L
#'
#' @returns nothing, updates specified candidate dataframe in place
update_cdf <- function(rawPitchDB, plotted_file, which_frame, new_candidate_i) {
  cdf <- rawPitchDB$cdf[[plotted_file]]
  this_frame <- which(cdf[["frame_i"]] == which_frame)
  these_candidates <- this_frame[cdf[["cand_i"]][this_frame] %in% c(1L, new_candidate_i)]
  new_indices <- rev(cdf[["cand_i"]][these_candidates])
  cdf[these_candidates, "cand_i" := new_indices]
}


get_vals_to_change <- function(selectedPoints, plotSubset) {
  vals_to_change <- selectedPoints$data$pulse_id # pulse ID for the full dataset == row id
  plot_vals_to_change <- match(selectedPoints$data$pulse_id, plotSubset$data$pulse_id) # ensures correct order
  plot_vals_to_change <- plot_vals_to_change[!is.na(plot_vals_to_change)]
  n_to_change <- length(vals_to_change)

  list(LF  = vals_to_change,
       PS = plot_vals_to_change,
       n = length(vals_to_change))
}

get_vals_to_change.voicing <- function(selectedPoints, plotSubset) {
  vals_to_change <- selectedPoints$data$pulse_id # pulse ID for the full dataset == row id
  is_voiced <- selectedPoints$data$is_voiced

  plot_vals_to_change <- match(vals_to_change, plotSubset$data$pulse_id) # ensures correct order
  plot_vals_to_change <- plot_vals_to_change[!is.na(plot_vals_to_change)]
  n_to_change <- length(vals_to_change)

  list(
    any_voiced   = any(is_voiced),
    any_unvoiced = any(!is_voiced),
    voiced   = list(LF  = vals_to_change[is_voiced],
                    PS = plot_vals_to_change[is_voiced],
                    n = length(vals_to_change[is_voiced])),
    unvoiced = list(LF  = vals_to_change[!is_voiced],
                    PS = plot_vals_to_change[!is_voiced],
                    n = length(vals_to_change[!is_voiced])),
    LF = vals_to_change,
    PS = plot_vals_to_change,
    n = length(vals_to_change)
  )


}
