octaveShiftSauceUI <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    shiny::actionButton(width = "48%",
                        inputId = ns("halfButton"),
                        label = "Halve Pulses",
                        title = "Click to halve point y-values",
                        style = "margin: 1%;margin-top:0%;margin-bottom:0"),
    shiny::actionButton(width = "48%",
                        inputId = ns("doubleButton"),
                        label = "Double Pulses",
                        title = "Click to double point y-values",
                        style = "margin: 1%;margin-top:0%;margin-bottom:0"))
}

# undoTransformUI <- function(id) {
#   ns <- NS(id)
#   shiny::actionButton(
#     inputId = ns("undoTransformButton"),
#     title = "Click to undo last doubling/halving transform",
#     label = "Undo Transform"
#   )
# }

octaveShiftSauceServer <- function(id,
                              loadedFile,
                              plotSubset,
                              transformedColumn,
                              selectedPoints,
                              lastTransformation,
                              getBrushedPoints,
                              updatePlot,
                              yValColumnInput,
                              pitchRangeInput,
                              lockButton,
                              rawPitchDB,
                              parent_session) {
  moduleServer(id, function(input, output, session) {

    get_and_set_new_freq_values <- function(octave) {
      octave <- rlang::arg_match0(octave, c("halve", "double"))
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- selectedPoints$data$pulse_id # pulse ID for the full dataset == row id
      plot_vals_to_change <- match(selectedPoints$data$pulse_id, plotSubset$data$pulse_id) # ensures correct order
      plot_vals_to_change <- plot_vals_to_change[!is.na(plot_vals_to_change)]

      # TODO: some of this information needs to be retained in lastTransformation
      #       since it can't just be multiplied by the inverse ratio anymore
      subset_to_change <- loadedFile$data[vals_to_change,]
      n_to_change <- length(vals_to_change)
      new_frame_values <- numeric(n_to_change)

      # TODO: this may need to be changed to a for loop to populate three
      #       different vectors so they can be saved in lastTransformation
      new_freq_values <-
        vapply(seq_len(n_to_change),
               \(i) {
                 file    <- subset_to_change[[i, "file"]]
                 frame_i <- subset_to_change[[i, "frame_i"]]
                 frame <- rawPitchDB$data[[file]][["frame"]][[frame_i]]
                 swapFrameValue(frame, 1L, get_value_octave_away(frame, octave))
               }, numeric(1))

      loadedFile$data[vals_to_change, f0 := new_freq_values]
      plotSubset$data[plot_vals_to_change, f0 := new_freq_values]

      new_freq_values
    }

    doublePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      new_freq_values <- get_and_set_new_freq_values("double")

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change
      current_pitch_range <- isolate(pitchRangeInput())
      max_transform_value <- max(new_freq_values, na.rm = TRUE)
      if (lockButton() %% 2 == 0 && current_pitch_range[2L] < max_transform_value){
        new_pitch_max <-
          ceiling(add_semitones(max_transform_value, sign(max_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session = parent_session, "pitchRangeInput",
                                              value = c(current_pitch_range[1L],
                                                        new_pitch_max))
      }
      updatePlot()

    })

    halvePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)

      new_freq_values <- get_and_set_new_freq_values("double")

      current_pitch_range <- isolate(pitchRangeInput())
      min_transform_value <- min(new_freq_values[where_not_zero(new_freq_values)],
                                 na.rm = TRUE)
      if (lockButton() %% 2 == 0 && current_pitch_range[1L] > min_transform_value){
        new_pitch_min <-
          ceiling(add_semitones(min_transform_value, sign(-min_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session = parent_session, "pitchRangeInput",
                                              value = c(new_pitch_min,
                                                        current_pitch_range[2L]))
      }
      updatePlot()
    })


    # TODO: this needs to be completely reworked with lastTransformation
    #       updated to have more information saved regarding files, frame_ids, etc.
    undoTransformation <- reactive({
      if (is.null(loadedFile$data) || is.null(lastTransformation$pulse_ids))
        return(NULL)
      plot_vals_to_change <- plotSubset$data$pulse_id %in% loadedFile$data$pulse_id[lastTransformation$pulse_ids]

      loadedFile$data[lastTransformation$pulse_ids, pulse_transform := 1.0]
      loadedFile$data[lastTransformation$pulse_ids, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]
      plotSubset$data[plot_vals_to_change, pulse_transform := 1.0]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]

      lastTransformation$pulse_ids <- NULL
      selectedPoints$data <- NULL

      updatePlot()
    })

    # Undo the last transformation, resetting the transformation to 1
    shiny::observeEvent(input$undoTransformButton, {
      message("Undo Pressed")
      undoTransformation()
    })


    # Multiply selected points by 2 (fixes halving errors)
    shiny::observeEvent(input$doubleButton, {
      message("Double Pressed")
      doublePulses()
    })



    # Multiply selected points by 0.5 (fixes doubling errors)
    shiny::observeEvent(input$halfButton, {
      message("Halve Pressed")
      halvePulses()
    })

    return(list(halvePulses = halvePulses,
                doublePulses = doublePulses,
                undoTransformation = undoTransformation))
  })
}
