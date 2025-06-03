#' Flag errors wrapper
#'
#' Flags potential sample-to-sample errors in F0 data. Uses a modified version
#' of the algorithm described in Steffman & Cole (2022). The implementaion here
#' is much faster than the original version and avoids NA issues and
#' overprocessing short files when longer files exist.
#'
#' @param data Data frame
#' @param .unique_file Column name containing unique file IDs
#' @param .hz Column name containing F0 in Hz
#' @param .time Column name containing timepoint
#' @param .samplerate Sample rate in milliseconds (usually 10ms)
#' @param .add_semitones Add semitones to F0
#' @param .speaker Column name containing speaker IDs. Set to NA to compute
#' semitones from the mean of each individual file. Set to NULL to use the
#' entire dataset to compute the mean.
#' @param rise_threshold Rise threshold to use for errors
#' @param fall_threshold Fall threshold to use for errors
#' @param .as_vec Whether just the results should be returned as a logical vector
#' (TRUE) or if the whole dataframe should be returned (FALSE)
#' @param .ignore_0s If TRUE, will separate the dataset into two parts: where
#' .hz is equal to 0 or NA, and where it is not (i.e., actual F0 values). The
#' algorithm will run with the latter set, then the former will be merged back
#' into the final dataset. This way the missing values are ignored, but the
#' results are of the same size as the input. If this is set to FALSE, then
#' you will get more false positives due to having many more F0 jumps from a
#' starting value of 0.
#' @param .window_size Numeric multiplier of the sample rate to ignore pulses.
#' For example, if you want to ignore pulses that are greater than 1 sample rate
#' away, leave as 1. If you want to ignore intervals of 80ms and the sampling
#' rate is 10ms, set to 8.
#' @param .verbose Logical, default FALSE, whether to print messages
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
                                  fall_threshold = -1.7142857143,
                                  .as_vec = FALSE,
                                  .ignore_0s = TRUE,
                                  .window_size = 8,
                                  .verbose = FALSE) {
  # Ensure that the f0 column is numeric in case missing values are
  # encoded as --undefined-- from Praat output
  data[[.hz]] <- as.numeric(data[[.hz]]) # Coerces non-numeric strings to NA!
  datalist <- .convert_time_to_ms(data, .time, .samplerate, .verbose = .verbose)
  trans_time_column <- paste0(.time, "___trans")

  data[[trans_time_column]] <- datalist[['time']]

  if (.ignore_0s) {
    requireNamespace("data.table", quietly = TRUE)
    # We need a unique id to use; the load file button will add this for us
    # but just in case it's not there we need to have it available
    if (!"pulse_id" %in% colnames(data))
      data[['pulse_id']] <- seq_len(nrow(data))

    # Split the data into two separate tables: the actual data, and where
    # there was no F0 detected (encoded as either 0 or NA).
    data[["where_not_zero__"]] <- where_not_zero(data[[.hz]])

    # Using data.table split method
    if (data.table::is.data.table(data))
      data_split <- split(data, by= 'where_not_zero__')
    else
      data_split <- split(data, ~ data[['where_not_zero__']])

    if (!"TRUE" %in% names(data_split))
      stop("No non-zero values found")

    updated_df <-
      data_split[["TRUE"]] |>
      annotate_errors(.unique_file = .unique_file,
                      .hz = .hz,
                      .time = trans_time_column,
                      .samplerate = datalist[['samplerate']],
                      .add_semitones = .add_semitones,
                      .speaker = .speaker,
                      .window_size = .window_size) |>
      code_carryover_effects(.unique_file = .unique_file,
                             .hz = .hz,
                             .time = trans_time_column,
                             rise_threshold = rise_threshold,
                             fall_threshold = fall_threshold) |>
      dplyr::ungroup()

    # If we had any bad values in our dataset, we need to
    # make the columns match those of the actual dataset
    if ("FALSE" %in% names(data_split)) {
      data_split[["FALSE"]][['flagged_samples']] <- TRUE
      data_split[["FALSE"]][["F0_semitones"]] <- NA

      updated_df <- rbind(updated_df, data_split[['FALSE']])
    }

    updated_df[["where_not_zero__"]] <- NULL
    updated_df <- dplyr::arrange(updated_df, pulse_id)
  } else {

    updated_df <-
      data |>
      annotate_errors(.unique_file = .unique_file,
                      .hz = .hz,
                      .time = trans_time_column,
                      .samplerate = datalist[['samplerate']],
                      .add_semitones = .add_semitones,
                      .speaker = .speaker,
                      .window_size = .window_size) |>
      code_carryover_effects(.unique_file = .unique_file,
                             .hz = .hz,
                             .time = trans_time_column,
                             rise_threshold = rise_threshold,
                             fall_threshold = fall_threshold) |>
      dplyr::ungroup()

  }

  updated_df[[trans_time_column]] <- NULL

  if (.as_vec)
    return(updated_df[['flagged_samples']])

  updated_df
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
#' @param only_samplerate Whether to return only the sampling rate, default FALSE
#' @param .verbose Logical, default FALSE, whether to print messages
#'
#' @return A list containing the transformed time and the sampling
#' rate in milliseconds
.convert_time_to_ms <- function(data, .time, .samplerate, only_samplerate = FALSE, .verbose=FALSE) {
  # If the sampling rate is not specified, we need to try and calculate
  # it ourselves
  if (is.na(.samplerate))
    .samplerate <- round(stats::median(diff(data[[.time]])), 4)

  transformed_time <- data[[.time]]

  # If the differences are less than 1 then the time is probably recorded
  # in seconds, so we need to convert to milliseconds
  if (.samplerate < 1) {
    example_value <- data[[.time]][1]
    transformed_time <- transformed_time * 1000
    .samplerate <-  .samplerate * 1000
    if(.verbose)
      message("Converting `", .time,  "` to milliseconds",
              " (", example_value, "s->", example_value*1000, "ms)")
  }

  if (only_samplerate)
    return(list('samplerate' = .samplerate))

  list('time' = transformed_time,
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
#' @param .speaker Column name containing speaker IDs. Set to NA to compute
#' semitones from the mean of each individual file. Set to NULL to use the
#' entire dataset to compute the mean.
#' @param .window_size Numeric multiplier of the sample rate to ignore pulses.
#' For example, if you want to ignore pulses that are greater than 1 sample rate
#' away, leave as 1. If you want to ignore intervals of 80ms and the sampling
#' rate is 10ms, set to 8.
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
                            .speaker = NULL,
                            rise_threshold = 1.2631578947,
                            fall_threshold = -1.7142857143,
                            .window_size = 1) {


  time_mutation <-  .samplerate/10

  if (is.na(.add_semitones)) {
    .add_semitones <- !"F0_semitones" %in% colnames(data)
  }

  if (!.add_semitones) {
    if (!"F0_semitones" %in% colnames(data))
      stop("F0_semitones column not found. Please add column or set .add_semitones to TRUE or NA.")
  }

  if (.add_semitones) {
    if (!is.null(.speaker)){
      if (is.na(.speaker)){
        warning("No speaker column provided. Will calculate semitones from mean of individual files.")
        data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(.unique_file)))
      } else if (!.speaker %in% colnames(data)) {
        stop(".speaker not found in data")
      }  else {
        data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(.speaker)))
      }
    }
    data <-
      data |>
      dplyr::mutate(F0_semitones = hz_to_semitones(.data[[.hz]], .semitones_from = mean(.data[[.hz]], na.rm=TRUE)))
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.unique_file))) |>
    dplyr::mutate(
      err = code_initial_errors(f0_semitones = F0_semitones,
                                time = .data[[.time]],
                                time_mutation = time_mutation,
                                samplerate = .samplerate,
                                windowsize = .window_size,
                                rise_threshold,
                                fall_threshold),
      F0_of_err = F0_semitones *  c(0, err[-length(err)])
    )
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
                                   fall_threshold = -1.7142857143) {
  data_annotated <-
    data_annotated |>
    dplyr::mutate(F0_of_err = propagate_f0_of_err(F0_of_err),
                  is_threshold_error = code_threshold_error(F0_semitones,
                                                            F0_of_err,
                                                            rise_threshold,
                                                            fall_threshold),
                  flagged_samples = propagate_while_true(err,
                                                         is_threshold_error)) |>
    dplyr::select(-is_threshold_error,
                  -F0_of_err,
                  -err)

  data_annotated
}

#' Code carryover threshold errors
#'
#' Helper to code whether a sample is within the tolerance range for
#' carryover errors.
#'
#' @param f0_st F0 in semitones
#' @param f0_err F0 of flagged errors
#' @param rise_threshold Positive rise threshold
#' @param fall_threshold Negative fall threshold
#'
#' @return Logical vector for threshold errors
#' @export
code_threshold_error <- function(f0_st,
                                 f0_err,
                                 rise_threshold =  1.2631578947,
                                 fall_threshold = -1.7142857143) {
  n_points <- length(f0_st)
  stopifnot(n_points == length(f0_err))

  f0_st_diff <- (f0_st - f0_err)
  is_rise <-diff(f0_st) > 0

  fall_threshold <- fall_threshold * 1.5
  rise_threshold <- rise_threshold * 1.5

  is_threshold_error <-
    vapply(seq_len(n_points-1L),
           \(i) {

             # if rise within rise threshold or fall within fall threshold
             if (is_rise[i])
               return(f0_st_diff[i] < rise_threshold)

             f0_st_diff[i] > fall_threshold

           },
           logical(1))

  is_threshold_error[n_points] <- FALSE

  is_threshold_error
}

#' Code initial threshold errors
#'
#' @param f0_semitones Numeric vector
#' @param time Numeric vector
#' @param time_mutation Mutation factor
#' @param samplerate Sampling rate, usually 10
#' @param windowsize Buffer size
#' @param rise_threshold Positive rise threshold
#' @param fall_threshold Negative fall threshold
#'
#' @return Logical vector, TRUE if error
code_initial_errors <- function(f0_semitones,
                                time,
                                time_mutation,
                                samplerate,
                                windowsize = 8,
                                rise_threshold = 1.2631578947,
                                fall_threshold = -1.7142857143) {
  n <- length(time)

  scaled_window <- samplerate * windowsize

  sample_diffs <- diff(f0_semitones)
  time_diffs <- diff(time)

  is_error <- vapply(seq_len(n-1L),
                     \(i) {
                       passes_threshold <-
                         (sample_diffs[i] > rise_threshold) ||
                         (sample_diffs[i] < fall_threshold)
                       passes_threshold && (time_diffs[i] <= scaled_window)
                     }, logical(1))


  is_error[n] <- FALSE

  is_error
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
  i <- 1L
  n <- length(F0_of_err)
  while (i <= n) {
    cur_value <- F0_of_err[i]
    if (cur_value == 0) {
      F0_of_err[i] <- prev_value
    } else{
      F0_of_err[i] <- cur_value
    }

    prev_value <- F0_of_err[i]
    i <- i + 1L
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

  i <- 1L
  n <- length(carryover_err)
  while (i <= n) {
    should_propogate <- (is_threshold_err[i] & prev_carryover_err)

    if (!is.na(should_propogate) & should_propogate){
      new_vals[i] <- prev_carryover_err
    }else{
      new_vals[i] <- carryover_err[i]
      prev_carryover_err <- carryover_err[i]
    }

    i <- i + 1L
  }

  new_vals[length(carryover_err)] <-  carryover_err[length(carryover_err)]
  new_vals
}

