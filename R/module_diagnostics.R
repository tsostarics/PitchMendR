diagnosticsUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Progress",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(""),
      bslib::card_body(
        shiny::actionButton(
          inputId = ns("refreshProgressButton"),
          title = "Click to recompute summary statistics on this page",
          label = "Refresh Diagnostics"
        ),
        shiny::actionButton(
          inputId = ns("plotUneditedFilesButton"),
          title = "Click to send all unedited files to the editor pane",
          label = "Send Unedited Files to Editor"
        ),
        "Percentage of pulses removed from current dataset:",
        shiny::verbatimTextOutput(outputId = ns("percentRemovedText")),
        "Average variance of sample-to-sample semitone differences (+/- 2 se, averaged across all files and normalized to sampling period)",
        shiny::verbatimTextOutput(outputId = ns("changeInVarianceOutput")),
        "Total proportion of files that have been mended:",
        shiny::verbatimTextOutput(outputId = ns("nEditedFilesText")),
        "These files have received no edits:",
        shiny::verbatimTextOutput(outputId = ns("uneditedFilesText"))
      )
    )
  )
}

diagnosticsServer <- function(id,
                              loadedFile,
                              fileHandler,
                              transformedColumn,
                              xValColumnInput,
                              yValColumnInput,
                              filenameColumnInput,
                              selectionColumnInput,
                              refilterSubset,
                              updatePlot
                              ) {
  moduleServer(id, function(input, output, session) {
    uneditedFiles <- shiny::reactiveValues(filenames = NULL)

    # Refresh the diagnostics pane when the user clicks the refresh button
    shiny::observeEvent(input$refreshProgressButton,{
      if (is.null(loadedFile$data))
        return(NULL)
      # browser()
      filtered_data <- loadedFile$data[where_not_zero(get(yValColumnInput())),]

      uneditedFiles$filenames <-
        filtered_data[, .(n_edited = any(!get(selectionColumnInput()))), by = c(filenameColumnInput())][!(n_edited), .SD[[filenameColumnInput()]]]


      output$percentRemovedText <- shiny::renderText({
        paste0(round(sum(!filtered_data[[selectionColumnInput()]]) / nrow(filtered_data)*100, 2), "%")
      })

      output$nEditedFilesText <- shiny::renderText({
        n_files <- length(fileHandler$filenames)
        n_edited <- n_files - length(uneditedFiles$filenames)
        paste0(n_edited, " / ", n_files)
      })

      output$changeInVarianceOutput <- shiny::renderText({
        samplerate <- median(diff(loadedFile$data[[xValColumnInput()]][1:100]))

        variance_values <-
          loadedFile$data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(filenameColumnInput()))) |>

          dplyr::reframe(
            original_variance = .var_of_diffs(.data[[yValColumnInput()]][where_not_zero(.data[[yValColumnInput()]])],
                                              .data[[xValColumnInput()]],
                                              samplerate),
            new_variance = .var_of_diffs(.data[[transformedColumn$name]][.data[[selectionColumnInput()]] & where_not_zero(.data[[yValColumnInput()]])],
                                         .data[[xValColumnInput()]],
                                         samplerate)) |>
          dplyr::ungroup()

        mean_se1 <- ggplot2::mean_se(variance_values$original_variance, mult = 2)
        mean_se2 <- ggplot2::mean_se(variance_values$new_variance, mult = 2)

        paste0("Original Variance: ", round(mean_se1['y'], 3), "[", round(mean_se1['ymin'], 3), ",", round(mean_se1['ymax'], 3), "]\n",
               "New Variance: ", round(mean_se2['y'], 3), "[", round(mean_se2['ymin'], 3), ",", round(mean_se2['ymax'], 3), "]\n")

      })

      output$uneditedFilesText <- shiny::renderText({

        paste(uneditedFiles$filenames, collapse = "\n")
      })

    })

    # When the user clicks the plot unedited files button, plot only the files
    # that the user has not edited. Must be done after the user has refreshed
    # the diagnostics pane
    shiny::observeEvent(input$plotUneditedFilesButton, {
      if(is.null(loadedFile$data) || is.null(uneditedFiles$filenames))
        return(NULL)

      fileHandler$isPlotted <- fileHandler$filenames %in% uneditedFiles$filenames
      refilterSubset()
      updatePlot()
    })

  })
}
