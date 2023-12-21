#' Flag errors wrapper
#'
#' Flags potential sample-to-sample errors in F0 data. Uses a modified version
#' of the algorithm described in Steffman & Cole (2022). The original algorithm
#' skips over pairs of samples that are further apart than the sampling rate to
#' account for voiceless intervals, but this is a bit too stringent for the
#' purposes of this app. The version here is a bit more lenient, only skipping
#' pairs that are more than 8 times the sampling rate apart (typically, 80ms).
#' The multiplier is a ballpark estimate based on some productions of /s/ in fast
#' speech. What this buys us is identifying regions of interest that may
#' contain errors. The implementaion here is also much faster than the original
#' version and doesn't rely on NAs as much.
#'
#' @param data Data frame
#' @param .unique_file Column name containing unique file IDs
#' @param .hz Column name containing F0 in Hz
#' @param .time Column name containing timepoint
#' @param .samplerate Sample rate in milliseconds (usually 10ms)
#' @param .add_semitones Add semitones to F0
#' @param .speaker Column name containing Speaker ID
#' @param rise_threshold Rise threshold to use for errors
#' @param fall_threshold Fall threshold to use for errors
#'
#' @return Dataframe with potential errors coded in the `flagged_samples` column
#' @export
#' @references
#'  - Jeremy Steffman, Jennifer Cole; An automated method for detecting F measurement jumps based on sample-to-sample differences. JASA Express Lett. 1 November 2022; 2 (11): 115201. https://doi.org/10.1121/10.0015045
#'  - https://github.com/kirbyj/praatsauce
#'  - https://github.com/jsteffman/f0-jumps
#'
#' @importFrom rlang .data
flag_potential_errors <- function(data,
                                  .unique_file = "uniqueID",
                                  .hz = "hz",
                                  .time = 'timepoint',
                                  .samplerate = NA,
                                  .add_semitones = NA,
                                  .speaker = NA,
                                  rise_threshold = 1.2631578947,
                                  fall_threshold = 1.7142857143) {

  datalist <- .convert_time_to_ms(data, .time, .samplerate)

  datalist[['df']] |>
    annotate_errors(.unique_file = .unique_file,
                    .hz = .hz,
                    .time = .time,
                    .samplerate = datalist[['samplerate']],
                    .add_semitones = .add_semitones,
                    .speaker = .speaker) |>
    code_carryover_effects(.unique_file = .unique_file,
                           .hz = .hz,
                           .time = .time,
                           rise_threshold = rise_threshold,
                           fall_threshold = fall_threshold) |>
    dplyr::ungroup()
}

#' Convert seconds (probably) to milliseconds
#'
#' The flag samples algorithm assumes that the time is in milliseconds, but it
#' is not uncommon for time to be specified in seconds. This function converts
#' the time to milliseconds if it is not already in milliseconds and automatically
#' tries to detect the sampling rate if it is not specified. The sampling rate
#' is determined by getting the median of the sample-to-sample time differences,
#' as it's most likely that the VAST majority of these will be the sampling rate
#'
#' @param data Dataframe
#' @param .time column name to use for time
#' @param .samplerate sampling rate for the unit of time in the dataframe,
#' so for a 10ms sampling rate with times in ms, this should be 10; if it's
#' recorded in seconds, this should be 0.01
#'
#' @return A list containing the data with the converted time and the sampling
#' rate in milliseconds
.convert_time_to_ms <- function(data, .time, .samplerate) {
  # If the sampling rate is not specified, we need to try and calculate
  # it ourselves
  if (is.na(.samplerate))
    .samplerate <- round(median(diff(data[[.time]])), 4)


  # If the differences are less than 1 then the time is probably recorded
  # in seconds, so we need to convert to milliseconds
  if (.samplerate < 1) {
    example_value <- data[[.time]][1]
    data[[.time]] <- data[[.time]] * 1000
    .samplerate <-  .samplerate * 1000
    message("Converting `", .time,  "` to milliseconds",
            " (", example_value, "s->", example_value*1000, "ms)")
  }


  list('df' = data,
       'samplerate' = .samplerate)
}

#' Annotated F0 data via threshold method
#'
#' This function annotates F0 data using the threshold method described in
#' Steffman and Cole (2022). The implementation here omits diagnostic columns
#' that are not used in the analysis.
#'
#' Note that this function is intended to be used with the output of PraatSauce
#'
#' @param data Dataframe
#' @param .hz Column name containing F0 values in Hz
#' @param .time Column name containing timepoint values
#' @param .samplerate The sampling rate of the data in milliseconds
#' @param .add_semitones If NA, will automatically check to see if `F0_semitones`
#' is present in the dataframe. If it is not, and `.speaker` is not NA, the
#' Hz values will be transformed to semitones using the mean of the speaker.
#' If `.speaker` is NA, the Hz values will be transformed to semitones using
#' the mean of the unique file ID. If `.add_semitones` is TRUE, the Hz values
#' will always be transformed to semitones, overwriting values in `F0_semitones`
#' if it exists. If `.add_semitones` is FALSE, the Hz values will not be
#' transformed to semitones, even if `F0_semitones` is present in the dataframe.
#' @param rise_threshold Rise threshold to use for errors
#' @param fall_threshold Fall threshold to use for errors
#' @param .unique_file Column name containing unique file IDs
#' @param .speaker Column name containing speaker IDs
#'
#' @return Dataframe with annotated F0 data
#' @export
#' @references
#'  - Jeremy Steffman, Jennifer Cole; An automated method for detecting F measurement jumps based on sample-to-sample differences. JASA Express Lett. 1 November 2022; 2 (11): 115201. https://doi.org/10.1121/10.0015045
#'  - Sundberg, J. (1973). “Data on maximum speed of pitch changes,” Speech Transm. Lab. Q. Prog. Status Rep.4, 39–47.
#'  - https://github.com/kirbyj/praatsauce
#'  - https://github.com/jsteffman/f0-jumps
annotate_errors <- function(data,
                            .unique_file = "uniqueID",
                            .hz = "hz",
                            .time = 'timepoint',
                            .samplerate = NA,
                            .add_semitones = NA,
                            .speaker = NA,
                            rise_threshold = 1.2631578947,
                            fall_threshold = 1.7142857143) {


  time_mutation <-  .samplerate/10

  if (is.na(.add_semitones)) {
    .add_semitones <- !"F0_semitones" %in% colnames(data)
  }

  if (!.add_semitones) {
    if (!"F0_semitones" %in% colnames(data))
      stop("F0_semitones column not found. Please add column or set .add_semitones to TRUE or NA.")
  }


  if (.add_semitones) {
    if (is.na(.speaker)){
      warning("No speaker column provided. Will calculate semitones from mean of individual files.")
      .speaker <- .unique_file
    }
    data <-
      data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.speaker))) |>
      dplyr::mutate(F0_semitones = hz_to_semitones(.data[[.hz]], .semitones_from = mean(.data[[.hz]], na.rm=TRUE)))
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.unique_file))) |>
    dplyr::mutate(
      lead_F0_semitones= dplyr::lead(F0_semitones, order_by=.data[[.time]]),
      lead_F0_Hz= dplyr::lead(.data[[.hz]], order_by=.data[[.time]]),
      diff = lead_F0_semitones-F0_semitones,
      time_diff = dplyr::lead(.data[[.time]]) - .data[[.time]],
      ratio_Hz = lead_F0_Hz/.data[[.hz]],
      err = ifelse(time_diff > .samplerate*8,0, ## this ignore time differences larger than 8*the time step, e.g., over voiceless intervals.
                   as.integer(diff>0&(abs(diff)*time_mutation)>rise_threshold|
                                diff<0&(abs(diff)*time_mutation)>fall_threshold)),
      # err_prop_by_ID = mean(err, na.rm = TRUE),
      # err_count_by_ID = sum(err, na.rm = TRUE),
      # err_in_ID = as.numeric(err_prop_by_ID > 0),
      # time_of_err = ifelse(err_in_ID & err, t_ms, 0),
      F0_of_err = ifelse(dplyr::lag(err)==1,F0_semitones,0),
      # oct_jump_prop_by_ID = mean(oct_jump, na.rm = TRUE),
      # oct_jump_count_by_ID = sum(oct_jump, na.rm = TRUE),
      # oct_jump_in_ID = as.integer(oct_jump_prop_by_ID > 0)
    ) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c('F0_of_err', 'err')), ~replace(., is.na(.), 0))) |>
    dplyr::select(-lead_F0_semitones, -lead_F0_Hz, -diff, -time_diff, -ratio_Hz)
}

#' Code carrryover pitch errors
#'
#' This function codes adjacent pitch pulses as a potential error if there was
#' previously a jump in pitch. For example:
#'
#' ```
#'  Octave jump
#'            ↓
#'            OOOO <-the next three should be coded as errors == "carryover errors"
#'           |    \
#'          |      \
#' OOOOOOOOO        OOOOOOOOO
#'
#'
#' Note that potential mistakes arise in cases like below:
#'
#'
#' O
#'  \  All of these might be coded as carryover errors
#'  \    ↓↓↓↓
#'  \↓↓↓↓OOOO↓↓↓↓
#'  OOOOO    OOOO↓↓↓↓↓
#'  ↑            OOOOO
#' Jump               OOOO <- These might be far enough away (vertically) to
#'                            stop being coded as potential errors
#' ```
#'
#' @param data_annotated Annotated data frame from `annotate_errors()`
#' @param .unique_file Column name containing unique file IDs
#' @param .hz Column name containing F0 values in Hz
#' @param .time Column name containing timepoint values
#' @param rise_threshold Rise threshold to use for errors
#' @param fall_threshold Fall threshold to use for errors
#'
#' @return A data frame with a new column `flagged_samples` that codes
#'        potential errors as 1 and non-errors as 0.
#' @export
code_carryover_effects <- function(data_annotated,
                                   .unique_file = "uniqueID",
                                   .hz = "hz",
                                   .time = 'timepoint',
                                   rise_threshold = 1.2631578947,
                                   fall_threshold = 1.7142857143) {
  data_annotated <-
    data_annotated |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.unique_file))) |>
    dplyr::mutate(carryover_err = dplyr::lag(err),
                  F0_of_err=propagate_f0_of_err(F0_of_err))

  data_annotated[["F0_of_err"]][is.na(data_annotated[["F0_of_err"]])] <- 0


  data_annotated <-
    dplyr::mutate(data_annotated,
                  next_F0_st = dplyr::lead(F0_semitones),
                  f0_st_diff = abs(F0_semitones - F0_of_err),
                  is_rise_error = (next_F0_st > F0_semitones & f0_st_diff < (rise_threshold * 1.5)),
                  is_fall_error = (next_F0_st < F0_semitones & f0_st_diff < (fall_threshold * 1.5)),
                  is_threshold_error = is_rise_error | is_fall_error,
                  is_threshold_error = ifelse(is.na(is_threshold_error), 0, is_threshold_error),
                  carryover_err = propagate_while_true(carryover_err, is_threshold_error))

  data_annotated[["carryover_err"]][is.na(data_annotated[["carryover_err"]])] <- 0

  data_annotated |>
    dplyr::mutate(carryover_err = ifelse(.data[[.time]]==max(.data[[.time]]) & dplyr::lag(carryover_err) == 1,1, carryover_err),
                  flagged_samples = carryover_err,
                  # flagged_samples includes the seeding samples for carryover error detection- i.e. some actual errors
                  carryover_only = ifelse(err==1,0,carryover_err),
                  prop_carryover_err = mean(carryover_only)) |>
    dplyr::select(-next_F0_st,
                  -carryover_err,
                  -f0_st_diff,
                  -is_rise_error,
                  -is_fall_error,
                  -is_threshold_error,
                  -carryover_only,
                  -F0_of_err)
}

#' Propagate F0 errors rightward
#'
#' If the current F0 value is 0, look at the previous value and use that instead.
#' This proceeds from left to right, propagating errors forward through sequences
#' of 0s. This replaces the step in the original implementation where this was
#' calculated through repeated lead/lag transformations.
#'
#' @param F0_of_err Numeric vector, F0_of_err column
#'
#' @return Numeric vector
propagate_f0_of_err <- function(F0_of_err) {
  prev_value <- F0_of_err[1]
  for (i in seq_along(F0_of_err)) {
    cur_value <- F0_of_err[i]

    F0_of_err[i] <- ifelse(cur_value==0, prev_value, cur_value)
    prev_value <- F0_of_err[i]
  }

  F0_of_err
}


#' Propagate while logical condition is TRUE
#'
#' Given a vector of carryover error assignments and a logical vector denoting
#' whether values are a rise or fall error, propagate carryover values through
#' sequences of TRUE values. This replaces the step in the original implementation
#' where this was calculated through repeated lead/lag transformations.
#'
#' @param carryover_err Numeric vector, carryover_err column
#' @param is_threshold_err Logical vector, is_threshold_err column
#'
#' @return Numeric vector
propagate_while_true <- function(carryover_err, is_threshold_err) {
  new_vals <- carryover_err[NA]
  new_vals[1] <- carryover_err[1]
  prev_carryover_err <- carryover_err[1]

  for (i in seq_along(carryover_err)) {
    should_propogate <- (is_threshold_err[i] & prev_carryover_err)

    if (!is.na(should_propogate) & should_propogate){
      new_vals[i] <- prev_carryover_err
    }else{
      new_vals[i] <- carryover_err[i]
      prev_carryover_err <- carryover_err[i]
    }
  }

  new_vals[length(carryover_err)] <-  carryover_err[length(carryover_err)]
  new_vals
}

