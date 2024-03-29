fileNavUI <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    shiny::actionButton(inputId = ns("prevButton"),
                        title = "Click to go to previous file alphabetically",
                        label = "<",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%"),
    shiny::actionButton(inputId = ns("saveButton"),
                        title = "Click to save annotated file to disk",
                        class="btn-default",
                        label = "Save File",
                        icon = icon("floppy-disk"),
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "38%"),
    shiny::actionButton(inputId = ns("nextButton"),
                        title = "Click to go to next file alphabetically",
                        label = ">",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%", )
  )
}

fileNavServer <- function(id,
                          loadedFile,
                          fileHandler,
                          saveOptionButton,
                          skipCheckedFilesToggle,
                          outputDirInput,
                          fileSelectBox,
                          filenameColumnInput,
                          nPlotted,
                          annotations,
                          refilterSubset,
                          destroyLoadedAudio,
                          fileDelimiter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # When the user clicks the save button, save the data to the output directory
    saveIcon <- reactiveValues(value = "floppy-disk")
    # observe({
    #   shiny::updateActionButton(session, "saveButton", icon = icon(saveIcon$value))
    # })

    saveData <- function(){
      if (is.null(loadedFile$data))
        return(NULL)

      annotations$mergeAnnotations()
      path <- file.path(outputDirInput(), clean_file(fileSelectBox()))


      names(fileHandler$fileChecked) <- fileHandler$filenames
      # Update the file checked column before saving
      loadedFile$data[, file_checked := fileHandler$fileChecked[loadedFile$data[[filenameColumnInput()]]]]
      if (!file.exists(path) || file.access(path, mode = 2) == 0) {
        shiny::updateActionButton(session, "saveButton", icon = icon("spinner"))
        write_status <- tryCatch(data.table::fwrite(x = loadedFile$data,
                                                    file = path,sep = fileDelimiter()),
                                 error = \(e) {
                                   e
                                 })

        if (!is.null(write_status)) {
          message(write_status$message)
          # saveIcon$value <- "triangle-exclamation"
          shiny::updateActionButton(session, "saveButton", icon = icon("triangle-exclamation"))

          return(NULL)
        }
        message(paste0("Wrote data to ", path))
        # saveIcon$value <- "floppy-disk"
        shiny::updateActionButton(session, "saveButton", icon = icon("floppy-disk"))

      }

    }
    #)

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

      saveData()
    })

    goToPreviousFile <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # Get the minimum index of the files that are currently plotted,
      # if we're already at the first file, wrap around to the last file
      current_min <- min(which(fileHandler$isPlotted))
      # if (current_min == 1)
      #   current_min <- length(fileHandler$filenames) + 1

      #$ Check off the file that's currently plotted before we move to the next file
      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      fileHandler$isPlotted[] <- FALSE

      # If we want to skip files we've already checked and there remain files
      # that haven't been checked, keep cycling through files until we find
      # the first unchecked file we can get to.
      if (skipCheckedFilesToggle() && !all(fileHandler$fileChecked)) {
        while (fileHandler$fileChecked[current_min]) {
          current_min <- current_min - 1

          # Wrap around to the other end if we've reached the beginning
          if (current_min < 1) # == 0
            current_min <- length(fileHandler$filenames)
        }
      }else {
        current_min <- current_min - 1
        if (current_min < 1) # == 0
          current_min <- length(fileHandler$filenames)
      }

      fileHandler$isPlotted[current_min] <- TRUE


      if (saveOptionButton()) {
        saveData()
      }

      refilterSubset()
      annotations$updateBadges()
      annotations$updateNotes()
      destroyLoadedAudio()

    })

    goToNextFile <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # Get the maximum index of the files that are currently plotted,
      # if we're already at the last file, wrap around to the first file
      current_max <- max(which(fileHandler$isPlotted))

      # Check off the file that's currently plotted before we move to the next file
      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      fileHandler$isPlotted[] <- FALSE
      nfiles <- length(fileHandler$filenames)

      if (skipCheckedFilesToggle() && !all(fileHandler$fileChecked)) {
        while (fileHandler$fileChecked[current_max]) {
          current_max <- current_max + 1

          # Wrap around to the other end if we've reached the beginning
          if (current_max > nfiles) # == 0
            current_max <- 1
        }
      } else {
        current_max <- current_max + 1
        if (current_max > nfiles) # == 0
          current_max <- 1
      }


      fileHandler$isPlotted[current_max] <- TRUE


      # If we have the save-on-next option enabled, save the data
      if (saveOptionButton()) {
        saveData()
      }
      refilterSubset()
      annotations$updateBadges()
      annotations$updateNotes()
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
                "goToPreviousFile" = goToPreviousFile,
                "saveData" = saveData))
  }

  )
}
