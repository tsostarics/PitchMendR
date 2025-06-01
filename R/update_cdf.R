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
