loadSauceFileUI <- function(id, input_directory, output_directory) {
  ns <- NS(id)

  tagList(

    "Current working directory:",
    shiny::verbatimTextOutput(outputId = ns("cwd")),
    tags$span(title = "Enter the directory containing spreadsheet to load",
              shiny::textInput(
                inputId = ns("inputDirInput"),
                label =  "Dataset input directory",
                value = input_directory,
                width = "100%"
              )),
    tags$span(title = "Enter the directory to save annotated spreadsheet to",
              shiny::textInput(
                inputId = ns("outputDirInput"),
                label =  "Output directory for annotated datasets",
                value = output_directory,
                width = "100%"
              )),
    "Number of files in input and output directories:",
    shiny::verbatimTextOutput(outputId = ns("nfiles")),
    # tags$span(title = "Select a file to load from the input directory",
    #           shiny::selectizeInput(inputId = ns("fileSelectBox"),
    #                                 label =  "Files Available (*=not processed yet)",
    #                                 multiple = FALSE,
    #                                 choices = NULL)),
    tags$span(title = "Use value >1 to load files in parallel.",
              shiny::numericInput(inputId = ns("numCoresInput"),
                                  label =  paste0("Enter the number of cores (max: ",
                                                  parallel::detectCores()-1L,
                                                  ") to use to load files in parallel."),
                                  value = 1,
                                  min = 1,
                                  max = parallel::detectCores() - 1L,
                                  step = 1L)),
    shiny::actionButton(
      inputId = ns("loadFileButton"),
      title = "Click to load selected file",
      label = "Load File",
      class = "btn-warning",
      icon = icon("spinner"),
      width = "100%",
      style = "margin-bottom:8px"
    )
  )

}

# flagSamplesButton_UI <- function(id) {
#   ns <- NS(id)
#   shiny::actionButton(
#     inputId = ns("flagSamplesButton"),
#     title = "Click to flag potential errors in loaded dataset",
#     icon = icon('flag'),
#     width = "100%",
#     label = "Flag Samples"
#   )
# }

loadSauceFile_workingFileOutput <- function(id) {
  ns <- NS(id)
  shiny::textOutput(outputId = ns("workingFileOutput"))
}


loadSauceFileServer <- function(id,
                                parent_session,
                                loadedFile,
                                rawPitchDB,
                                # fileSelectBox,
                                # inputDirInput,
                                # outputDirInput,
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
                                updatePlot
                                # numCoresInput
                                ) {
  moduleServer(id, function(input, output, session) {

    output$cwd <- shiny::renderText({
      message("Rendering cwd")
      getwd()
    })

    observe({
      if(!is.null(input$inputDirInput) && !is.null(input$outputDirInput)) {
        input_filepaths <- list.files(input$inputDirInput, ".Pitch$", include.dirs = FALSE)
        # input_filepaths <- input_filepaths[!grepl("exe|png|jpg|jpeg|svg|pdf|tiff|bmp|wav|zip|msi$", input_filepaths,ignore.case = TRUE)]
        input_filenames <- basename(input_filepaths)
        hasOutput <- input_filenames %in% list.files(input$outputDirInput, include.dirs = FALSE)

        output$nfiles <- shiny::renderText({
          paste0("Input: ", length(input_filepaths), ", Output: ", sum(hasOutput))
        })

        #
        # shiny::updateSelectizeInput(session,
        #                             inputId = "fileSelectBox",
        #                             choices = paste0(input_filepaths, c("*", "")[hasOutput+1]))
      }
    })


    # fileDelimiter <- shiny::reactiveVal(value=",")
    shiny::observeEvent(input$loadFileButton, {
      message("Load File Pressed")
      # Don't run if no file is selected
      if (is.null(input$inputDirInput)){
        return(NULL)
      }

      # Don't run if input directory doesn't exist
      if (!dir.exists(input$inputDirInput)){
        message("Input directory doesnt exist")
        return(NULL)
      }


      message("Loading files")
      loadedFile$data <- NULL # If previous data was loaded, throw it out
      rawPitchDB$data <- NULL #

      # browser()
      # TODO: Go through files to read and check the output directory to see
      #       if there are any files that have been edited already
      files_to_read <- list.files(input$inputDirInput, pattern = "Pitch$", full.names = TRUE)
      filenames <- basename(files_to_read)
      if (input$numCoresInput > 1L) {
        requireNamespace("future")
        requireNamespace("furrr")
        future::plan("multisession", workers = input$numCoresInput)
        rawPitchDB$data <- furrr::future_map(files_to_read, pitch.read2)
        names(rawPitchDB$data) <- filenames
        future::plan("sequential")

      } else {
        rawPitchDB$data <- lapply(files_to_read, pitch.read2)
        names(rawPitchDB$data) <- filenames
      }


      loadedFile$data <-
        data.table::rbindlist(
          lapply(filenames,
                 \(x) {
                   find_path(rawPitchDB$data[[x]], x)
                 })
        )

      # Upon successful load of the file, change the color of the load file button
      # so it doesn't stand out as much anymore
      shinyjs::removeClass("loadFileButton", "btn-primary")

      # Add animations to some of the important buttons
      shinyjs::addClass("fileNav-saveButton", class = "animbutton")
      shinyjs::addClass("checkVisibleFilesButton", class = "animbutton")

      # TODO: INPUTS NOT NEEDED, PITCH OBJS WILL HAVE STANDARDIZED TABULAR FORMAT


      # Arrange the measurement points for each file in order just in case
      # they aren't already

      # TODO: HARD CODE COLUMNS HERE, SORTING MIGHT NOT EVEN BE NECESSARY
      # if (!selectionColumnInput() %in% loaded_colnames)
        loadedFile$data[, keep_pulse := where_not_zero(f0)]

      # Forcefully add pulse id, overwrites in instances where the
      # ordering gets messed up
      loadedFile$data[, pulse_id := .I]

      # TODO: REWORK PULSE TRANSFORM LOGIC TO ACCOMODATE CANDIDATES
      # if (!"pulse_transform" %in% loaded_colnames){
      #   loadedFile$data[, pulse_transform := 1.0]
      # }
      #
      # changeTransformedColumn()

      # Update the fileHandler with the information from the loaded file
      fileHandler$filenames <- filenames
      fileHandler$isPlotted <- rep(TRUE, length(fileHandler$filenames)) # upon file load, plot all files
      fileHandler$indices <-
        lapply(fileHandler$filenames,
               \(fname) which(loadedFile$data[["file"]] == fname)) |>
        `names<-`(fileHandler$filenames)

      # Add the file_checked column if it doesn't exist. If it does,
      # then validate that the column has no missing values and update
      # the fileHandler with which files have already been checked.
      # if (!"file_checked" %in% loaded_colnames) {
      #   loadedFile$data[, file_checked := FALSE]
        fileHandler$fileChecked <- rep(FALSE, length(fileHandler$filenames))
      # } else {
      #   loaded_file_check <- loadedFile$data[, .(file_checked = ifelse(is.na(file_checked[1]), FALSE, file_checked[1])), by = c(filenameColumnInput())]
      #
      #   file_checks <- loaded_file_check$file_checked
      #   names(file_checks) <- loaded_file_check[[filenameColumnInput()]]
      #   fileHandler$fileChecked <- file_checks[fileHandler$filenames]
      # }

      plotSettings$showLine <- TRUE
      output$workingFileOutput <- shiny::renderText({
        paste0("Working File:\n", clean_file(fileSelectBox()))
      })
      # If we've loaded another file in the same session then we need
      # to reset the flag samples button. If flagged_samples already exists,
      # then set the button to success and use that column for the color coding,
      # otherwise remove the success class and reset the color coding column
      # TODO: CHECK THIS
      # if ("flagged_samples" %in% loaded_colnames) {
      #   shinyjs::addClass(id = "flagSamplesButton", class = "btn-success")
      #   shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
      #   shinyWidgets::updateMaterialSwitch(parent_session, "useFlaggedColumnToggle", value = TRUE)
      #   set_selectize_choices(parent_session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      # } else {
      #   shinyjs::removeClass(id = "flagSamplesButton", class = "btn-success") # Will remove if it exists, otherwise takes no effect
      #   shiny::updateActionButton(session, "flagSamplesButton", icon = icon("flag"))
      #   set_selectize_choices(session, "colorCodeColumnInput", loadedFile, colorCodeColumnInput())()
      # }

      loadedFile$data[["flagged_samples"]] <- FALSE

      # If we're using annotation badges, then add the tags column if it doesn't
      # already exist. Otherwise update the fileHandler's badges with what's
      # in the loaded file dataframe.
      # if (useBadgesToggle()) {
      #   if (!"tags" %in% loaded_colnames) {
      #     loadedFile$data[, tags := NA_character_]
      #     fileHandler$badges <- rep(NA_character_, length(fileHandler$filenames))
      #     names(fileHandler$badges) <- fileHandler$filenames
      #   } else {
      #     # The tags are held in a column which means that each file has its
      #     # tags listed N times for N rows even though they're the same value.
      #     # So, we only need to take the first one.
      #     fileHandler$badges = loadedFile$data[, .(tags = tags[1]), by = c(filenameColumnInput())][['tags']]
      #     names(fileHandler$badges) <- fileHandler$filenames
      #     loadedFile$data[,tags := as.character(tags)]
      #   }
      # }

      # If we're using the notes field, then add the notes column if it doesn't
      # alreadye exist. Otherwise, load the notes from the dataframe the same
      # way we loaded the tags.
      # if (useNotesToggle()) {
      #   if (!"notes" %in% colnames(loadedFile$data)) {
      #     loadedFile$data[, notes := NA_character_]
      #     fileHandler$notes <- rep(NA_character_, length(fileHandler$filenames))
      #     names(fileHandler$notes) <- fileHandler$filenames
      #   } else {
      #     fileHandler$notes = loadedFile$data[, .(notes = notes[1]), by = c(filenameColumnInput())][['notes']]
      #     names(fileHandler$notes) <- fileHandler$filenames
      #     loadedFile$data[,notes := as.character(notes)]
      #   }
      # }

      # Now that we've loaded the file, we can force update the plot
      # browser()
      refilterSubset()
      updatePlot()
      message("File loading complete")
      # message(paste0("Delimiter: '", fileDelimiter(), "'"))
    })

    # When the user clicks the Flag Samples button, all files in the loaded
    # dataset will be checked for potential errors. These are added to the
    # flagged_samples column. So the user can can tell that the process worked,
    # the button will change color and the icon will change to a checkmark.
    shiny::observeEvent(input$flagSamplesButton, {
      message("Flag Samples Pressed")
      if (is.null(loadedFile$data))
        return(NULL)

      shinyjs::addClass("flagSamplesButton", class = "btn-warning")

      flagged_values <-
        flag_potential_errors(loadedFile$data,
                              .unique_file = "file",
                              .hz = "f0",
                              .time = "t",
                              .samplerate = NA,
                              .speaker = NULL,
                              .as_vec = TRUE)

      loadedFile$data[, ("flagged_samples") := flagged_values]

      if (!data.table::is.data.table(loadedFile$data))
        loadedFile$data <- data.table(loadedFile$data)

      shinyjs::removeClass("flagSamplesButton", class = "btn-warning")
      shinyjs::addClass(id = 'flagSamplesButton',class = "btn-success")
      shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
      shinyWidgets::updateMaterialSwitch(parent_session, "useFlaggedColumnToggle", value = TRUE)
      set_selectize_choices(parent_session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      refilterSubset()

    })

    # Reactively update the appearance of the loadfile button
    observe({
      if (!is.null(input$inputDirInput)) {
        shinyjs::removeClass("loadFileButton", class = "btn-warning")
        shinyjs::addClass("loadFileButton", class = "btn-primary")
        shiny::updateActionButton(session, "loadFileButton",icon = icon("upload"))
      } else {
        shinyjs::removeClass("loadFileButton", class = "btn-primary")
        shinyjs::addClass("loadFileButton", class = "btn-warning")
        shiny::updateActionButton(session, "loadFileButton",icon = icon("spinner"))
      }
    })

    return(list(fileDelimiter = NA_character_))
  })
}
