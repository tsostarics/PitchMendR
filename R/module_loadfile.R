loadFileUI <- function(id) {
  ns <- NS(id)
  shiny::actionButton(
    inputId = ns("loadFileButton"),
    title = "Click to load selected file",
    label = "Load File",
    class = "btn-warning",
    icon = icon("spinner"),
    width = "100%",
    style = "margin-bottom:8px"
  )
}

loadFile_workingFileOutput <- function(id) {
  ns <- NS(id)
  shiny::textOutput(outputId = ns("workingFileOutput"))
}


loadFileServer <- function(id,
                           loadedFile,
                           fileSelectBox,
                           inputDirInput,
                           outputDirInput,
                           filenameColumnInput,
                           xValColumnInput,
                           yValColumnInput,
                           selectionColumnInput,
                           colorCodeColumnInput,
                           useBadgesToggle,
                           useNotesToggle,
                           changeTransformedColumn,
                           fileHandler,
                           plotSettings,
                           refilterSubset,
                           updatePlot) {
  moduleServer(id, function(input, output, session) {
    fileDelimiter <- shiny::reactiveVal(value=",")

    shiny::observeEvent(input$loadFileButton, {
      message("Load File Pressed")
      # Don't run if no file is selected
      if (is.null(fileSelectBox()) | is.null(inputDirInput())){
        return(NULL)
      }

      # Don't run if input directory doesn't exist
      if (!dir.exists(inputDirInput())){
        message("Input directory doesnt exist")
        return(NULL)
      }

      # Process the file input and output paths. If the output file already
      # exists, then resume progress with that file
      file_to_load <- file.path(inputDirInput(), clean_file(fileSelectBox()))
      outFile <- file.path(outputDirInput(), clean_file(fileSelectBox()))
      if (file.exists(outFile)) {
        message("Resuming progress from existing output file")
        file_to_load <- outFile
      }

      message(paste0("Loading file ", file_to_load))
      loadedFile$data <- NULL # If previous data was loaded, throw it out

      # Load the file using data.table::fread, which will automatically try to
      # detect the appropriate delimiter for the file in the freadMain C call.
      # The separator is printed as part of the verbose messaging, so we're going
      # to exploit that so we can save the delimiter when we go to save the file
      # later on. Otherwise, we run into an issue where we might load a tsv file
      # correctly, but it will be saved as a .tsv file that's actually comma
      # delimited, since the default sep for fwrite is ','.
      fread_output <- utils::capture.output({loadedFile$data <- data.table::fread(file_to_load, verbose=TRUE)})

      # Separator is on a line like:
      #   sep=','  with  // = , delimiter
      #   sep=0x9  with  // = \t delimiter, needs to be converted to a character
      sep_output <- fread_output[grepl("sep=.+?  with", fread_output)]
      delimiter <- regmatches(sep_output,regexec("sep='?([^']*?)'?  ",sep_output))[[1]][2]

      # If we've identified a delimiter successfully, then check to see if it
      # needs to be converted from a hex value to a character value. If that's
      # successful, or if it's just a single character to start with, use that
      # as the new delimiter.
      if (!is.na(delimiter) & !is.null(delimiter)) {
        if (nchar(delimiter) > 1) {
          converted_delimiter <- intToUtf8(delimiter)
          if (is.na(converted_delimiter)) {
            stop(paste0("Malformed delimiter '", delimiter,"'. Delimiter must be hex value like '0x9' or single string character like ','"))
          }
          delimiter <- converted_delimiter
        }
        fileDelimiter(delimiter) # Update delimiter with the one we found
      }

      # Upon successful load of the file, change the color of the load file button
      # so it doesn't stand out as much anymore
      shinyjs::removeClass("loadFileButton", "btn-primary")

      # Add animations to some of the important buttons
      shinyjs::addClass("fileNav-saveButton", class = "animbutton")
      shinyjs::addClass("checkVisibleFilesButton", class = "animbutton")

      # Given the loaded file, set the dropdown box options with the column names
      set_selectize_choices(session, "filenameColumnInput", loadedFile, filenameColumnInput())()
      set_selectize_choices(session, "xValColumnInput", loadedFile, xValColumnInput())()
      set_selectize_choices(session, "yValColumnInput", loadedFile, yValColumnInput())()


      # If the file doesn't contain the specified columns, return null and move
      # to the settings page
      if (!all(c(filenameColumnInput(), xValColumnInput(), yValColumnInput()) %in% colnames(loadedFile$data))) {
        message("File doesn't contain the specified columns")
        loadedFile$data <- NULL
        shiny::updateNavbarPage(session, "navbar", "Settings")
        return(NULL)
      }

      # Arrange the measurement points for each file in order just in case
      # they aren't already
      data.table::setorderv(loadedFile$data, cols = c(filenameColumnInput(), xValColumnInput()))

      if (!file.exists(outFile))
        loadedFile$data[, (selectionColumnInput()) := where_not_zero(get(yValColumnInput()))]

      # Add pulse id if it doesn't already exist
      if (!"pulse_id" %in% colnames(loadedFile$data))
        loadedFile$data[, pulse_id := .I]

      if (!"pulse_transform" %in% colnames(loadedFile$data)){
        loadedFile$data[, pulse_transform := 1.0]
      }

      changeTransformedColumn()

      # Update the fileHandler with the information from the loaded file
      fileHandler$filenames <- unique(loadedFile$data[[filenameColumnInput()]])
      fileHandler$isPlotted <- rep(TRUE, length(fileHandler$filenames)) # upon file load, plot all files
      # TODO: I believe we're not using this indices logic anymore
      fileHandler$indices <-
        lapply(fileHandler$filenames,
               \(fname) which(loadedFile$data[[filenameColumnInput()]] == fname)) |>
        `names<-`(fileHandler$filenames)

      # Add the file_checked column if it doesn't exist. If it does,
      # then validate that the column has no missing values and update
      # the fileHandler with which files have already been checked.
      if (!"file_checked" %in% colnames(loadedFile$data)) {
        loadedFile$data[, file_checked := FALSE]
        fileHandler$fileChecked <- rep(FALSE, length(fileHandler$filenames))
      } else {
        loaded_file_check <- loadedFile$data[, .(file_checked = ifelse(is.na(file_checked[1]), FALSE, file_checked[1])), by = c(filenameColumnInput())]

        file_checks <- loaded_file_check$file_checked
        names(file_checks) <- loaded_file_check[[filenameColumnInput()]]
        fileHandler$fileChecked <- file_checks[fileHandler$filenames]
      }

      plotSettings$showLine <- TRUE
      output$workingFileOutput <- shiny::renderText({
        paste0("Working File:\n", clean_file(fileSelectBox()))
      })

      # If we've loaded another file in the same session then we need
      # to reset the flag samples button. If flagged_samples already exists,
      # then set the button to success and use that column for the color coding,
      # otherwise remove the success class and reset the color coding column
      if ("flagged_samples" %in% colnames(loadedFile$data)) {
        shinyjs::addClass(id = "flagSamplesButton", class = "btn-success")
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
        shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      } else {
        shinyjs::removeClass(id = "flagSamplesButton", class = "btn-success") # Will remove if it exists, otherwise takes no effect
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("flag"))
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, colorCodeColumnInput())()
      }

      # If we're using annotation badges, then add the tags column if it doesn't
      # already exist. Otherwise update the fileHandler's badges with what's
      # in the loaded file dataframe.
      if (useBadgesToggle()) {
        if (!"tags" %in% colnames(loadedFile$data)) {
          loadedFile$data[, tags := NA_character_]
          fileHandler$badges <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$badges) <- fileHandler$filenames
        } else {
          # The tags are held in a column which means that each file has its
          # tags listed N times for N rows even though they're the same value.
          # So, we only need to take the first one.
          fileHandler$badges = loadedFile$data[, .(tags = tags[1]), by = c(filenameColumnInput())][['tags']]
          names(fileHandler$badges) <- fileHandler$filenames
          loadedFile$data[,tags := as.character(tags)]
        }
      }

      # If we're using the notes field, then add the notes column if it doesn't
      # alreadye exist. Otherwise, load the notes from the dataframe the same
      # way we loaded the tags.
      if (useNotesToggle()) {
        if (!"notes" %in% colnames(loadedFile$data)) {
          loadedFile$data[, notes := NA_character_]
          fileHandler$notes <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$notes) <- fileHandler$filenames
        } else {
          fileHandler$notes = loadedFile$data[, .(notes = notes[1]), by = c(filenameColumnInput())][['notes']]
          names(fileHandler$notes) <- fileHandler$filenames
          loadedFile$data[,notes := as.character(notes)]
        }
      }

      # Now that we've loaded the file, we can force update the plot
      refilterSubset()
      updatePlot()
      message(paste0("Delimiter: '", fileDelimiter(), "'"))
    })

    # Reactively update the appearance of the loadfile button
    observe({
      if (!is.null(fileSelectBox()) && !is.null(inputDirInput())) {
        shinyjs::removeClass("loadFileButton", class = "btn-warning")
        shinyjs::addClass("loadFileButton", class = "btn-primary")
        shiny::updateActionButton(session, "loadFileButton",icon = icon("upload"))
      } else {
        shinyjs::removeClass("loadFileButton", class = "btn-primary")
        shinyjs::addClass("loadFileButton", class = "btn-warning")
        shiny::updateActionButton(session, "loadFileButton",icon = icon("spinner"))
      }
    })

    return(list(fileDelimiter = fileDelimiter))
  })
}
