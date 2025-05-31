fileNavSauceUI <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    shiny::actionButton(inputId = ns("prevButton"),
                        title = "Click to go to previous file alphabetically",
                        label = "<",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%"),
    bslib::input_task_button(id = ns("saveButton"),
                             title = "Click to save annotated file to disk",
                             # class="btn-default",
                             label = "Save File",
                             label_busy = "Saving...",
                             icon = icon("floppy-disk"),
                             type = "default",
                             style = "margin:1%;margin-top:0%;margin-bottom:0;width:38%"),
    shiny::actionButton(inputId = ns("nextButton"),
                        title = "Click to go to next file alphabetically",
                        label = ">",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%", )
  )
}

fileNavSauceServer <- function(id,
                               loadedFile,
                               rawPitchDB,
                               fileHandler,
                               saveOptionButton,
                               skipCheckedFilesToggle,
                               outputDirInput,
                               # fileSelectBox,
                               filenameColumnInput,
                               nPlotted,
                               annotations,
                               refilterSubset,
                               destroyLoadedAudio,
                               fileDelimiter,
                               plotFlag) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Placeholder so we can create one-off event listeners
    saveObserver <- NULL

    saveData <- function(){
      if (is.null(loadedFile$data))
        return(NULL)

      if (file.access(outputDirInput(), mode = 2) == 0) { # TODO: check this
        shinyjs::addClass("saveButton", "btn-warning")

        # Save all of the files that have changed
        files_to_save <- fileHandler$filenames[fileHandler$hasChanged]
        filepaths <- file.path(outputDirInput(), files_to_save)
        n_to_save <- length(files_to_save)

        pbar <- Progress$new(session, min = 0, max = n_to_save)
        pbar$set(value = 0, message = paste0("Saving ", n_to_save, " files..."))
        # browser()
        write_status <-
          vapply(seq_along(files_to_save),
                 \(i) {
                   pbar$inc(1L)
                   rPraat::pitch.write(rawPitchDB$data[[files_to_save[i]]], filepaths[i])
                 },
                 1L)

        fileHandler$hasChanged[fileHandler$hasChanged][write_status == 0L] <- FALSE

        # TODO: Need some better error handling for failures here

        # if (!is.null(write_status)) {
        #   message(write_status$message)
        #   shiny::updateActionButton(session, "saveButton", icon = icon("triangle-exclamation"))
        #
        #   return(NULL)
        # }

        # TODO: Save summary csv file in output directory
        pbar$close()
        message(paste0("Saved ", length(write_status), " files"))
        # bslib::update_task_button(session, "saveButton", icon = icon("check"))
        shinyjs::removeClass("saveButton", "btn-warning")
        shinyjs::addClass("saveButton", "btn-success")
        # Create an observer that waits until the dataset is modified (denoted
        # by a change in plotFlag$value due to in-place data.table modifications).
        # Upon modification, the save button icon changes back to a floppy disk
        # to denote that there are unsaved changes
        saveObserver <<-
          shiny::observeEvent(
            list(plotFlag$value,
                 fileHandler$notes,
                 fileHandler$badges), {
                   shinyjs::removeClass("saveButton", "btn-success")
                 },
            once = TRUE,
            ignoreInit = TRUE
            )

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
        shiny::updateActionButton(session, "saveButton", icon = icon("triangle-exclamation"))
        return(NULL)
      }

      saveData()
    })

    goToPreviousFile <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # Get the minimum index of the files that are currently plotted,
      # if we're already at the first file, wrap around to the last file
      current_min <- min(which(fileHandler$isPlotted), na.rm = TRUE)

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
      current_max <- max(which(fileHandler$isPlotted), na.rm = TRUE)

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
