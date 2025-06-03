sauce_diagnosticsUI <- function(id) {
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
        "These files have notes or tags, click a row to immediately go to that file:",
        div(DT::dataTableOutput(ns("taggedFilesTable"))),
        "These files have received no edits:",
        shiny::verbatimTextOutput(outputId = ns("uneditedFilesText"))
      )
    )
  )
}

sauce_diagnosticsServer <- function(id,
                              loadedFile,
                              parent_session,
                              fileHandler,
                              transformedColumn,
                              xValColumnInput,
                              yValColumnInput,
                              filenameColumnInput,
                              selectionColumnInput,
                              refilterSubset,
                              updatePlot,
                              saveData,
                              destroyLoadedAudio,
                              annotations
) {
  moduleServer(id, function(input, output, session) {
    uneditedFiles <- shiny::reactiveValues(filenames = NULL)
    file_table <- shiny::reactiveValues(data = NULL)

    # Refresh the diagnostics pane when the user clicks the refresh button
    shiny::observeEvent(input$refreshProgressButton,{
      if (is.null(loadedFile$data))
        return(NULL)

      saveData()
      # browser()
      filtered_data <- loadedFile$data[where_not_zero(get("f0")),]

      uneditedFiles$filenames <-
        filtered_data[, .(n_edited = any(!get(selectionColumnInput()))), by = c("file")][!(n_edited), .SD[["file"]]]


      output$percentRemovedText <- shiny::renderText({
        paste0(round(sum(!filtered_data[[selectionColumnInput()]]) / nrow(filtered_data)*100, 2), "%")
      })

      output$nEditedFilesText <-
        shiny::renderText({
          n_files <- length(fileHandler$filenames)
          n_edited <- n_files - length(uneditedFiles$filenames)
          paste0(n_edited, " / ", n_files)
        })

      output$changeInVarianceOutput <- shiny::renderText({
        samplerate <- median(diff(loadedFile$data[["t"]][1:100]))

        variance_values <-
          loadedFile$data |>
          dplyr::group_by(dplyr::across(dplyr::all_of("file"))) |>

          dplyr::reframe(
            original_variance = .var_of_diffs(.data[["original_f0"]][where_not_zero(.data[["original_f0"]])],
                                              .data[["t"]],
                                              samplerate),
            new_variance = .var_of_diffs(.data[["f0"]][.data[["keep_pulse"]] & where_not_zero(.data[["f0"]])],
                                         .data[["t"]],
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

      file_table$data <-
        data.frame(file = fileHandler$filenames,
                   tags = fileHandler$badges,
                   notes = fileHandler$notes) |>
        dplyr::summarize(.by = c("file", tags, notes)) |>
        dplyr::filter((!is.na(notes) & notes != "") | (!is.na(tags) & tags != "")) |>
        dplyr::arrange("file")

      output$taggedFilesTable <-
        DT::renderDT({
          DT::datatable(file_table$data,
                        selection = list(mode = "single", target = "row")) |>
            DT::formatStyle(columns = c("file", 'tags', 'notes'), cursor = "pointer")
        })

      proxy <- DT::dataTableProxy('taggedFilesTable')

      observeEvent(input$taggedFilesTable_rows_selected, {
        # browser()
        if (is.null(input$taggedFilesTable_rows_selected))
          return(NULL)

        fileHandler$isPlotted <- fileHandler$filenames %in% file_table$data[input$taggedFilesTable_rows_selected,][["file"]]
        refilterSubset()
        updatePlot()
        DT::selectRows(proxy,selected = NULL)
        # DT::clearSearch(proxy)
        shiny::updateNavbarPage(parent_session, inputId = "navbar", selected = "Editor")
        annotations$updateBadges()
        annotations$updateNotes()
        destroyLoadedAudio()
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
