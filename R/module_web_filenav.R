web_fileNavUI <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    shiny::actionButton(inputId = ns("prevButton"),
                        title = "Click to go to previous file alphabetically",
                        label = "<",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%"),
    shiny::downloadButton(outputId = ns("saveButton"),
                          title = "Click to save annotated file to disk",
                          class="btn-default",
                          label = "Save File",
                          style = "margin:1%;margin-top:0%;margin-bottom:0;width:38%"),
    shiny::actionButton(inputId = ns("nextButton"),
                        title = "Click to go to next file alphabetically",
                        label = ">",
                        style = "margin:1%;margin-top:0%;margin-bottom:0",
                        width = "28%", )
  )
}

web_fileNavServer <- function(id,
                              loadedFile,
                              fileHandler,
                              # uploadInput,
                              # saveOptionButton,
                              skipCheckedFilesToggle,
                              # outputDirInput,
                              # fileSelectBox,
                              filenameColumnInput,
                              nPlotted,
                              annotations,
                              refilterSubset,
                              plotFlag) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- reactive({
      plotFlag
      annotations$mergeAnnotations()
      names(fileHandler$fileChecked) <- fileHandler$filenames
      loadedFile$data[, file_checked := fileHandler$fileChecked[loadedFile$data[[filenameColumnInput()]]]]
      loadedFile$data
    })

    # Merge annotations and download annotated file when download is clicked
    output$saveButton <- shiny::downloadHandler(
      filename = function() {
        # browser()
        # uploaded_filename <- basename(uploadInput()$datapath)
        timestamp <- format(Sys.time(), "_%Y_%m_%d_%H%M")
        paste0("PitchMendR", timestamp, ".csv")
      },
      content = function(file) {
        data.table::fwrite(x = data(), file = file)
      }
    )

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

      refilterSubset()
      annotations$updateBadges()
      annotations$updateNotes()

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

      refilterSubset()
      annotations$updateBadges()
      annotations$updateNotes()
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
