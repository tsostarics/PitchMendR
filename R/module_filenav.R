fileNavUI <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    shiny::actionButton(width = "28%", inputId = ns("prevButton"), label = "<", style = "margin:1%;margin-top:0%;margin-bottom:0"),
    shiny::actionButton(width = "38%", inputId = ns("saveButton"), class="btn-default",
                        label = shiny::uiOutput(outputId = ns("saveButtonLabel")),
                        style = "margin:1%;margin-top:0%;margin-bottom:0"),
    shiny::actionButton(width = "28%", inputId = ns("nextButton"), label = ">", style = "margin:1%;margin-top:0%;margin-bottom:0")
  )
}

fileNavServer <- function(id,
                          loadedFile,
                          fileHandler,
                          saveOptionButton,
                          outputDirInput,
                          fileSelectBox,
                          filenameColumnInput,
                          nPlotted,
                          annotations,
                          refilterSubset,
                          destroyLoadedAudio) {
  moduleServer(id, function(input, output, session) {

    saveIcon <- shiny::reactiveValues(value = "floppy-disk")

    output$saveButtonLabel <- shiny::renderUI({
      tags$span(shiny::icon(saveIcon$value), "Save File")
    })

    # When the user clicks the save button, save the data to the output directory
    saveData <- function(path) {
      if (is.null(loadedFile$data))
        return(NULL)

      # Update the file checked column before saving
      loadedFile$data[, file_checked := fileHandler$fileChecked[.SD[[filenameColumnInput()]]]]
      if (!file.exists(path) || file.access(path, mode = 2) == 0) {
        saveIcon$value <- "spinner"
        write_status <- tryCatch(data.table::fwrite(x = loadedFile$data, file = path),
                                 error = \(e) {
                                   e
                                 })

        if (!is.null(write_status)) {
          message(write_status$message)
          saveIcon$value <- "triangle-exclamation"
          return(NULL)
        }
        message(paste0("Wrote data to ", path))
        saveIcon$value <- "floppy-disk"
      }

    }

    # When the user clicks the save button, save the data to the output directory
    shiny::observeEvent(input$saveButton, {
      message("Save Pressed")
      if (is.null(loadedFile$data))
        return(NULL)

      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      if (!dir.exists(outputDirInput())){
        message("Output directory doesnt exist")
        saveIcon$value <- "triangle-exclamation"
        return(NULL)
      }

      saveData(file.path(outputDirInput(), clean_file(fileSelectBox())))
    })

    goToPreviousFile <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # Get the minimum index of the files that are currently plotted,
      # if we're already at the first file, wrap around to the last file
      current_min <- min(which(fileHandler$isPlotted))
      if (current_min == 1)
        current_min <- length(fileHandler$filenames) + 1

      #$ Check off the file that's currently plotted before we move to the next file
      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      fileHandler$isPlotted[] <- FALSE
      fileHandler$isPlotted[current_min - 1] <- TRUE
      annotations$updateBadges()
      annotations$updateNotes()

      if (saveOptionButton()) {
        saveData(file.path(outputDirInput(), clean_file(fileSelectBox())))
      }
      refilterSubset()
      destroyLoadedAudio()

    })

    goToNextFile <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # Get the maximum index of the files that are currently plotted,
      # if we're already at the last file, wrap around to the first file
      current_max <- max(which(fileHandler$isPlotted))
      if (current_max >= length(fileHandler$filenames))
        current_max <- 0

      # Check off the file that's currently plotted before we move to the next file
      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      fileHandler$isPlotted[] <- FALSE
      fileHandler$isPlotted[current_max + 1] <- TRUE
      annotations$updateBadges()
      annotations$updateNotes()

      # If we have the save-on-next option enabled, save the data
      if (saveOptionButton()) {
        saveData(file.path(outputDirInput(), clean_file(fileSelectBox())))
      }
      refilterSubset()
      destroyLoadedAudio()
    })


    # When the user clicks the next button, plot the next file alphabetically
    shiny::observeEvent(input$nextButton, {
      goToNextFile()

    })

    # When the user clicks the previous button, plot the previous file alphabetically
    shiny::observeEvent(input$prevButton, {
      goToPreviousFile()
    })

    return(list("goToNextFile" = goToNextFile,
                "goToPreviousFile" = goToPreviousFile))
  }

  )
  }