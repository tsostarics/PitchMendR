loadSauceFileUI <- function(id, input_directory, output_directory) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
                      .highlight-question {
                      box-shadow: 0px 0px 6px 3px cyan
                      }
                      "))
    ),
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
    singleton(tags$span(style = "display:inline-block",
                        "Number of files in input and output directories:",
                        tags$span(id = ns("dirQuestion"),
                                  style = "cursor:pointer",
                                  shiny::icon("circle-question")))),
    shiny::verbatimTextOutput(outputId = ns("nfiles")),
    tags$span(title = "Use value >1 to load files in parallel.",
              shiny::numericInput(inputId = ns("numCoresInput"),
                                  label =  paste0("Enter the number of cores (max: ",
                                                  parallel::detectCores()-1L,
                                                  ") to use to load files in parallel."),
                                  value = 1,
                                  min = 1,
                                  max = parallel::detectCores() - 1L,
                                  step = 1L)),
    bslib::input_task_button(
      id = ns("loadFileButton"),
      title = "Click to load Pitch files",
      label = "Load File",
      label_busy = "Loading...",
      type = "primary",
      icon = icon("upload"),
      width = "100%",
      style = "margin-bottom:8px"
    )
  )

}

loadSauceFileServer <- function(id,
                                parent_session,
                                loadedFile,
                                rawPitchDB,
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
) {
  moduleServer(id, function(input, output, session) {

    output$cwd <- shiny::renderText({
      getwd()
    })

    inputFiles <- shiny::reactiveValues(paths = NULL,
                                        names = NULL,
                                        n = NULL,
                                        hasOutput = NULL)

    nfiles_string <- shiny::reactiveVal(NULL)


    shinyjs::onclick(id = "dirQuestion", {
      shinyWidgets::show_alert(

        title = "Why can't I overwrite input files?",
        type = 'info',
        width = "35em",
        html = TRUE,
        text =
          tags$div(style = css(`text-align` = 'left'),
                   shiny::markdown(c("PitchMendR is designed to make *non-destructive* changes to files.",
                                     "If a .Pitch file is modified and then saved, it is not possible to determine what was modified.",
                                     "This makes reproduceability difficult and makes it hard to keep track of progress across multiple sessions.",
                                     "",
                                     "For these reasons, the input and output directories must be different when working with .Pitch files."))
          )
      )
    })


    update_inputFiles <- function(input, inputFiles, nfiles_string) {
      if(!is.null(input$inputDirInput) && !is.null(input$outputDirInput)) {
        inputFiles$paths <- list.files(input$inputDirInput, ".Pitch$",
                                       include.dirs = FALSE,
                                       full.names = TRUE)
        inputFiles$names <- basename(inputFiles$paths)
        inputFiles$n <- length(inputFiles$paths)
        inputFiles$hasOutput <- inputFiles$names %in% list.files(input$outputDirInput, include.dirs = FALSE)

        # Change which to read
        if (any(inputFiles$hasOutput)) {
          message(paste0("Found ", sum(inputFiles$hasOutput), " files in output directory."))
          inputFiles$paths[inputFiles$hasOutput] <-
            file.path(input$outputDirInput, inputFiles$names[inputFiles$hasOutput])
        }

      }


      if (input$outputDirInput == input$inputDirInput) {
        nfiles_string("Input & output dirs must be different.")
        shinyjs::addClass(id = "loadFileButton",class = "btn-secondary")
        shinyjs::removeClass(id = "loadFileButton", class = "btn-primary")
        shinyjs::disable(id = "loadFileButton")
        shinyjs::addClass(id = "dirQuestion", class = "highlight-question")

      } else {
        nfiles_string(paste0("Input: ", inputFiles$n, ", Output: ", sum(inputFiles$hasOutput)))
        shinyjs::enable(id = "loadFileButton")
        shinyjs::addClass(id = "loadFileButton",class = "btn-primary")
        shinyjs::removeClass(id = "loadFileButton", class = "btn-secondary")
        shinyjs::removeClass(id = "dirQuestion", class = "highlight-question")
      }

    }

    shiny::observeEvent(input$inputDirInput, {
      shinyjs::removeClass("loadFileButton", "btn-success")
      update_inputFiles(input, inputFiles, nfiles_string)
    })

    shiny::observeEvent(input$outputDirInput, {
      shinyjs::removeClass("loadFileButton", "btn-success")
      update_inputFiles(input, inputFiles, nfiles_string)
    },ignoreInit = TRUE)


    output$nfiles <- shiny::renderText({nfiles_string()})

    # fileDelimiter <- shiny::reactiveVal(value=",")
    shiny::observeEvent(input$loadFileButton, {
      message("Load File Pressed")
      # Don't run if no file is selected
      if (is.null(inputFiles$paths)){
        return(NULL)
      }

      # Don't run if input directory doesn't exist
      if (!dir.exists(input$inputDirInput)){
        message("Input directory doesnt exist")
        return(NULL)
      }

      if (!dir.exists(input$outputDirInput)){
        created <- dir.create(input$outputDirInput)
        if (!created) {
          stop("Output directory doesn't exist & could not be created.")
        } else {
          message("Created output directory")
        }
        # return(NULL) # doesn't seem right unless creation triggers another check
      }




      message("Loading files")
      loadedFile$data <- NULL # If previous data was loaded, throw it out
      rawPitchDB$data <- NULL #

      # browser()
      # TODO: Go through files to read and check the output directory to see
      #       if there are any files that have been edited already
      # browser()
      shinyjs::removeClass("loadFileButton", "btn-primary")
      shinyjs::addClass("loadFileButton", "btn-warning")

      pbar <- Progress$new(session, min = 0, max = inputFiles$n)

      if (input$numCoresInput > 1L) {
        requireNamespace("future")
        requireNamespace("furrr")
        future::plan("multisession", workers = input$numCoresInput)
        rawPitchDB$data <- furrr::future_map(inputFiles$paths, pitch.read2)
        names(rawPitchDB$data) <- inputFiles$names
        future::plan("sequential")

      } else {
        pbar$set(value = 0, message = paste0("Loading files..."))
        rawPitchDB$data <-
          lapply(inputFiles$paths, \(x) {
            pbar$inc(1L)
            pitch.read2(x)

          })

        names(rawPitchDB$data) <- inputFiles$names
      }

      loadedFile$data <-
        data.table::rbindlist(
          lapply(inputFiles$names,
                 \(x) {
                   find_path(rawPitchDB$data[[x]], x)
                 })
        )

      # Upon successful load of the file, change the color of the load file button
      # so it doesn't stand out as much anymore
      # shinyjs::removeClass("loadFileButton", "btn-primary")
      shinyjs::removeClass("loadFileButton", "btn-warning")
      shinyjs::addClass("loadFileButton", "btn-success")

      # Add animations to some of the important buttons
      shinyjs::addClass("fileNav-saveButton", class = "animbutton")
      shinyjs::addClass("checkVisibleFilesButton", class = "animbutton")

      # TODO: INPUTS NOT NEEDED, PITCH OBJS WILL HAVE STANDARDIZED TABULAR FORMAT


      # Arrange the measurement points for each file in order just in case
      # they aren't already

      # TODO: HARD CODE COLUMNS HERE, SORTING MIGHT NOT EVEN BE NECESSARY
      # if (!selectionColumnInput() %in% loaded_colnames)
      loadedFile$data[, keep_pulse := where_not_zero(f0)]
      loadedFile$data[, f0_i := 1L]

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
      fileHandler$filenames <- inputFiles$names
      fileHandler$isPlotted <- rep(TRUE, inputFiles$n) # upon file load, plot all files
      # browser()
      fileHandler$indices <-
        setNames(
          lapply(fileHandler$filenames,
                 \(fname) loadedFile$data[file == fname,][["pulse_id"]]),
          fileHandler$filenames
        )

      fileHandler$hasChanged <- setNames(rep(FALSE, inputFiles$n), fileHandler$filenames)

      # Add the file_checked column if it doesn't exist. If it does,
      # then validate that the column has no missing values and update
      # the fileHandler with which files have already been checked.
      # if (!"file_checked" %in% loaded_colnames) {
      #   loadedFile$data[, file_checked := FALSE]
      fileHandler$fileChecked <- setNames(rep(FALSE, inputFiles$n), fileHandler$filenames)
      fileHandler$fileChecked[inputFiles$hasOutput] <- TRUE
      # } else {
      #   loaded_file_check <- loadedFile$data[, .(file_checked = ifelse(is.na(file_checked[1]), FALSE, file_checked[1])), by = c(filenameColumnInput())]
      #
      #   file_checks <- loaded_file_check$file_checked
      #   names(file_checks) <- loaded_file_check[[filenameColumnInput()]]
      #   fileHandler$fileChecked <- file_checks[fileHandler$filenames]
      # }

      plotSettings$showLine <- TRUE
      # output$workingFileOutput <- shiny::renderText({
      #   paste0("Working File:\n", clean_file(fileSelectBox()))
      # })
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
      pbar$close()
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
    # observe({
    #   # browser()
    #   message("Observer triggered")
    #   if (!is.null(input$inputDirInput)) {
    #     shinyjs::removeClass("loadFileButton", class = "btn-warning")
    #     shinyjs::addClass("loadFileButton", class = "btn-primary")
    #     shiny::updateActionButton(session,inputId =  "loadFileButton",icon = icon("upload"))
    #   } else {
    #     shinyjs::removeClass("loadFileButton", class = "btn-primary")
    #     shinyjs::addClass("loadFileButton", class = "btn-warning")
    #     shiny::updateActionButton(session, inputId= "loadFileButton",icon = shiny::icon("spinner"))
    #   }
    # })

    return(list(outputDirInput = reactive(input$outputDirInput),
                inputDirInput  = reactive(input$inputDirInput)))
  })
}

