octaveShiftUI <- function(id) {
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

undoTransformUI <- function(id) {
  ns <- NS(id)
  shiny::actionButton(
    inputId = ns("undoTransformButton"),
    title = "Click to undo last doubling/halving transform",
    label = "Undo Transform"
  )
}

octaveShiftServer <- function(id,
                              loadedFile,
                              plotSubset,
                              transformedColumn,
                              selectedPoints,
                              lastTransformation,
                              getBrushedPoints,
                              updatePlot,
                              yValColumnInput,
                              pitchRangeInput,
                              lockButton) {
  moduleServer(id, function(input, output, session) {

    doublePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
      plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id

      loadedFile$data[vals_to_change, pulse_transform := pulse_transform * 2.0]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]
      plotSubset$data[plot_vals_to_change, pulse_transform := pulse_transform * 2.0]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change

      current_pitch_range <- isolate(pitchRangeInput())
      max_transform_value <- max(loadedFile$data[[transformedColumn$name]])
      if (lockButton() %% 2 == 0 && current_pitch_range[2L] < max_transform_value){
        new_pitch_max <-
          ceiling(add_semitones(max_transform_value, sign(max_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session, "pitchRangeInput",
                                              value = c(current_pitch_range[1L],
                                                        new_pitch_max))
      }
      updatePlot()

    })

    halvePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
      plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id

      # data.table likes to coerce to integers, but this makes 0.5 get truncated
      # to 0L, so we need to ensure that the columns are numeric
      if (typeof(loadedFile$data[1,pulse_transform]) == "integer")
        data.table::set(loadedFile$data, j= "pulse_transform", value=as.numeric(loadedFile$data[['pulse_transform']]))

      if (typeof(plotSubset$data[1,pulse_transform]) == "integer")
        data.table::set(plotSubset$data, j= "pulse_transform", value=as.numeric(plotSubset$data[['pulse_transform']]))


      loadedFile$data[vals_to_change, pulse_transform := pulse_transform * 0.5]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]
      plotSubset$data[plot_vals_to_change, pulse_transform := pulse_transform * 0.5]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(yValColumnInput()) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change

      current_pitch_range <- isolate(pitchRangeInput())
      min_transform_value <- min(loadedFile$data[[transformedColumn$name]][where_not_zero(loadedFile$data[[transformedColumn$name]])])
      if (lockButton() %% 2 == 0 && current_pitch_range[1L] > min_transform_value){
        new_pitch_min <-
          ceiling(add_semitones(min_transform_value, sign(-min_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session, "pitchRangeInput",
                                              value = c(new_pitch_min,
                                                        current_pitch_range[2L]))
      }
      updatePlot()
    })


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
