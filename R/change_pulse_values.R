
remove_pulses <- function(to_change,
                          loadedFile, plotSubset, rawPitchDB) {

  for (i in seq_len(to_change$n)) {
    id  <- to_change$LF[i]
    pid <- to_change$PS[i]

    file    <- loadedFile$data[["file"]][id]
    frame_i <- loadedFile$data[["frame_i"]][id]
    frame   <- rawPitchDB$data[[file]][["frame"]][[frame_i]]
    zi      <- loadedFile$data[["zero_index"]][id]

    # This is typically true, but if a transformation has been applied previously then it won't be
    if (loadedFile$data[["is_voiced"]][id]) {
      swapFrameValue(frame, 1L, zi)
      loadedFile$data[id,  "f0_i" := zi]
      plotSubset$data[pid, "f0_i" := zi]

      loadedFile$data[id,  "zero_index" := 1L]
      plotSubset$data[pid, "zero_index" := 1L]

      # If the candidate df has already been made (after viewing a file by
      # itself) then we need to also update the candidate indices in that.
      # If it hasn't been created yet, it will be handled automatically
      # due to how the cdf is created.
      if (!is.null(rawPitchDB$cdf[[file]])) {
        update_cdf(rawPitchDB, file, frame_i, zi)
      }
    }
  }
  to_change
  # set_values(to_change, FALSE)
}

set_values <- function(to_change, to = NA, loadedFile, plotSubset, selectedPoints, fileHandler) {
  # NA used as a third value for toggle_pulses
  if (is.na(to)) {
    loadedFile$data[to_change$LF, "keep_pulse" := !keep_pulse]
    plotSubset$data[to_change$PS, "keep_pulse" := !keep_pulse]
    loadedFile$data[to_change$LF, "is_voiced"  := !is_voiced]
    plotSubset$data[to_change$PS, "is_voiced"  := !is_voiced]
  } else {
    stopifnot(is.logical(to))
    loadedFile$data[to_change$LF, "keep_pulse" := to]
    plotSubset$data[to_change$PS, "keep_pulse" := to]
    loadedFile$data[to_change$LF, "is_voiced"  := to]
    plotSubset$data[to_change$PS, "is_voiced"  := to]
  }

  files_changed <- unique(loadedFile$data[["file"]][to_change$LF])
  fileHandler$hasChanged[files_changed] <- TRUE
  selectedPoints$data <- NULL
  to
}

swap_clicked_candidate <- function(to_change,
                               loadedFile, plotSubset, rawPitchDB, fileHandler, input) {
  plotted_file <- fileHandler$filenames[fileHandler$isPlotted]

  clicked_cdf <- shiny::nearPoints(rawPitchDB$cdf[[plotted_file]],
                                   input$plot_click,
                                   xvar = "t",
                                   yvar = "f0",
                                   threshold = 10L,
                                   maxpoints = 1L)

  # If the user clicks something not near a candidate
  if (is.null(clicked_cdf) | nrow(clicked_cdf) == 0) {
    return(NULL)
  }


  id <- plotSubset$data[frame_i == clicked_cdf[["frame_i"]],][["pulse_id"]]
  frame_i <- clicked_cdf[["frame_i"]]
  is_voiced <-  loadedFile$data$is_voiced[id]

  pid <- match(id, plotSubset$data$pulse_id)

  to_change <-
    list(LF = id,
         PS = pid,
         n = 1L)


  clicked_candidate <-  clicked_cdf[["cand_i"]]
  if (clicked_candidate == 1L) {
    # If the user clicks on the current f0 candidate (ie a dot or triangle)
    if (is_voiced) {
      # message("Removing clicked pulse")
      # Unvoice the clicked point
      remove_pulses(to_change, loadedFile, plotSubset, rawPitchDB)
      set_values(to_change, FALSE, loadedFile, plotSubset, list(data = clicked_cdf), fileHandler)

    } else {
      # Voice the clicked point
      # message("Voicing clicked pulse")
      frame <- rawPitchDB$data[[plotted_file]][["frame"]][[clicked_cdf[["frame_i"]]]]
      new_f0 <- swapFrameValue(frame, 1L, clicked_candidate)
      loadedFile$data[id,  "f0" := new_f0]
      plotSubset$data[pid, "f0" := new_f0]

      update_cdf(rawPitchDB, plotted_file, frame_i, clicked_candidate)

      loadedFile$data[id,  "f0_i" := 1L]
      plotSubset$data[pid, "f0_i" := 1L]

      loadedFile$data[id,  "zero_index" := clicked_candidate]
      plotSubset$data[pid, "zero_index" := clicked_candidate]

      set_values(to_change, TRUE, loadedFile, plotSubset, list(data = clicked_cdf), fileHandler)
    }
  } else {
    # message("Swapping clicked pulse")
    frame <- rawPitchDB$data[[plotted_file]][["frame"]][[clicked_cdf[["frame_i"]]]]

    # If the user clicks on a different candidate, then we need to swap the
    # F0 value to that candidate and voice the frame
    new_f0 <- swapFrameValue(frame, 1L, clicked_candidate)
    loadedFile$data[id,  "f0" := new_f0]
    plotSubset$data[pid, "f0" := new_f0]

    update_cdf(rawPitchDB, plotted_file, frame_i, clicked_candidate)
    set_values(to_change, TRUE, loadedFile, plotSubset, list(data = clicked_cdf), fileHandler)
  }

  to_change

}

keep_pulses_one <- function(to_change_unvoiced = NULL,
                            loadedFile, plotSubset, rawPitchDB, fileHandler, input) {
  plotted_file <- fileHandler$filenames[fileHandler$isPlotted]
  br_cdf <- shiny::brushedPoints(rawPitchDB$cdf[[plotted_file]],
                                 input$plot_brush,
                                 xvar = "t",
                                 yvar = "f0")
  # Summarize cdf to the max cand_i to avoid duplicates
  br_cdf <- br_cdf[br_cdf[, .I[cand_rank == max(cand_rank)], by = frame_i]$V1,]

  # Grab the pulse ids for this file, then only ids from the brushed frames
  frames <- br_cdf[["frame_i"]]
  vals_to_change <-
    loadedFile$data[["pulse_id"]][fileHandler$indices[[plotted_file]]][br_cdf$frame_i]

  if (!is.null(to_change_unvoiced)) {
    only_change_these <- vals_to_change %in% to_change_unvoiced$PS
    vals_to_change <- vals_to_change[only_change_these]
    frames <- frames[only_change_these]
  } else {
    to_change_unvoiced <- list(LF = vals_to_change,
                               PS = match(vals_to_change, plotSubset$data[["pulse_id"]]),
                               n = length(vals_to_change))
  }

  plot_vals_to_change <- match(vals_to_change, plotSubset$data$pulse_id) # ensures correct order
  for (i in seq_along(frames)) {
    id  <- vals_to_change[i]
    pid <- plot_vals_to_change[i]
    frame_i <- frames[i]

    frame <- rawPitchDB$data[[plotted_file]][["frame"]][[frame_i]]

    selected_cand <- br_cdf[["cand_i"]][i]

    # Nothing needs to be swapped if selected cand == current cand (always 1 for voiced)
    if (selected_cand == 1L)
      next

    # We need to update a few things:
    #  1: The raw pitch file in the database (swap candidate values)
    #  2: The table with all the data (new f0 val)
    #  3: The table with the currently plotted data (new f0 val)
    #  4: The candidate-level dataframe (swap candidate indices)
    new_f0 <- swapFrameValue(frame, 1L, selected_cand)
    loadedFile$data[id,  "f0" := new_f0]
    plotSubset$data[pid, "f0" := new_f0]

    update_cdf(rawPitchDB, plotted_file, frame_i, selected_cand)

    loadedFile$data[id,  "f0_i" := 1L]
    plotSubset$data[pid, "f0_i" := 1L]

    if (!loadedFile$data[["is_voiced"]][id]) {
      loadedFile$data[id,  "zero_index" := selected_cand]
      plotSubset$data[pid, "zero_index" := selected_cand]
    }
  }

  to_change_unvoiced
}

keep_pulses_multi <- function(to_change, loadedFile, plotSubset, rawPitchDB) {

  for (i in seq_len(to_change$n)) {
    id  <- to_change$LF[i]
    pid <- to_change$PS[i]

    file    <- loadedFile$data[["file"]][id]
    frame_i <- loadedFile$data[["frame_i"]][id]
    frame   <- rawPitchDB$data[[file]][["frame"]][[frame_i]]
    cur_f0_i <- loadedFile$data[["f0_i"]][id]
    # This is typically true, but if a transformation has been applied previously then it won't be
    if (!loadedFile$data[["is_voiced"]][id]) {
      swapFrameValue(frame, 1L, cur_f0_i)
      loadedFile$data[id,  "f0_i" := 1L]
      plotSubset$data[pid, "f0_i" := 1L]

      loadedFile$data[id,  "zero_index" := cur_f0_i]
      plotSubset$data[pid, "zero_index" := cur_f0_i]
    } else {
      if (cur_f0_i == 1L) {
        max_strength_i <- which.max(frame[["strength"]])
        swapFrameValue(frame, 1L, max_strength_i)
        loadedFile$data[id,  "zero_index" := max_strength_i]
        plotSubset$data[pid, "zero_index" := max_strength_i]

      }
      # f0_i doesn't change since the value from max_strength_i takes the first position
    }
  }

  to_change
}
