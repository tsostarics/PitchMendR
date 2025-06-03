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
                                fileHandler,
                                plotSettings,
                                refilterSubset,
                                updatePlot,
                                annotationDB
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

      update_inputFiles(input, inputFiles, nfiles_string)

      shinyjs::removeClass("loadFileButton", "btn-primary")
      shinyjs::addClass("loadFileButton", "btn-warning")

      pbar <- Progress$new(session, min = 0, max = inputFiles$n + sum(inputFiles$hasOutput))

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
      # loadedFile$data[, is_voiced := where_not_zero(f0)]
      loadedFile$data[, f0_i := 1L]

      loadedFile$data[, original_f0 := f0]

      # Forcefully add pulse id, overwrites in instances where the
      # ordering gets messed up
      loadedFile$data[, pulse_id := .I]

      # TODO: REWORK PULSE TRANSFORM LOGIC TO ACCOMODATE CANDIDATES
      # if (!"pulse_transform" %in% loaded_colnames){
      #   loadedFile$data[, pulse_transform := 1.0]
      # }
      #
      # changeTransformedColumn()

      # TODO: write faster version of pitch.readlines2 that only loads the first candidate values
      pbar$set(message = "Checking original values...")

      has_mismatching_n_frames <-
      vapply(inputFiles$names[inputFiles$hasOutput],
             \(infile) {
               fp <- file.path(input$inputDirInput, infile)
               original_f0_values <-
                 vapply(pitch.read2(fp)[["frame"]],
                        \(f) f[["frequency"]][1L],
                        1.0)
# browser()
               tryCatch({
                 this_file_pulse_ids <- loadedFile$data[file == infile,][["pulse_id"]]
                 loadedFile$data[this_file_pulse_ids, original_f0 := original_f0_values]
                 frames <- rawPitchDB$data[[infile]]$frame

                 which_are_zero_now <-
                   vapply(frames,
                        \(f) f$frequency[1L] < 1,
                        TRUE)
                 current_zero_pulse_ids <- this_file_pulse_ids[which_are_zero_now]

                 zero_frame_indices <- loadedFile$data[["frame_i"]][current_zero_pulse_ids]
                 original_f0_to_compare <- original_f0_values[which_are_zero_now]
                 # Which candidate has the original f0 value?
                 original_f0_candidate <-
                   vapply(seq_len(length(original_f0_to_compare)),
                        \(i) {
                          # print(i)
                          freq_diffs <-
                            frames[[zero_frame_indices[i]]]$frequency -  original_f0_to_compare[i]
                          which(abs(freq_diffs) < 1e-10)
                        }, 1L)

                 loadedFile$data[current_zero_pulse_ids, f0_i := original_f0_candidate]
                 loadedFile$data[current_zero_pulse_ids, zero_index := 1L]
                 loadedFile$data[current_zero_pulse_ids, f0 := original_f0_values[which_are_zero_now]]

                 return(FALSE)
                 }, error = \(e) return(TRUE))
             }, TRUE)

      if (any(has_mismatching_n_frames)) {
        shinyWidgets::show_alert(

          title = "File mismatch",
          type = 'error',
          width = "35em",
          html = TRUE,
          text =
            tags$div(style = css(`text-align` = 'left'),
                     shiny::markdown(c("These files were found in the input and output directories, but they did not have the same number of frames. I have loaded the file from the **output** directory.",
                                       "If you want to load the file from the input directory instead, please delete the file from the output directory or change the output directory.",
                                       "",
                                       "Did you create new Pitch files in the input directory using a different time step or window length since the last time you used PitchMendR with this dataset?",
                                       paste0(" - ", inputFiles$names[has_mismatching_n_frames])))
            )
        )
      }

      # Update the fileHandler with the information from the loaded file
      fileHandler$filenames <- inputFiles$names
      fileHandler$isPlotted <- rep(TRUE, inputFiles$n) # upon file load, plot all files
      fileHandler$indices <-
        setNames(
          lapply(fileHandler$filenames,
                 \(fname) loadedFile$data[file == fname,][["pulse_id"]]),
          fileHandler$filenames
        )

      fileHandler$hasChanged <- setNames(rep(FALSE, inputFiles$n), fileHandler$filenames)

      fileHandler$fileChecked <- setNames(rep(FALSE, inputFiles$n), fileHandler$filenames)
      fileHandler$fileChecked[inputFiles$hasOutput] <- TRUE

      plotSettings$showLine <- TRUE

      loadedFile$data[["flagged_samples"]] <- FALSE

      summary_filepath <- file.path(input$outputDirInput, "pitchmendr_notes.tsv")
      fileHandler$badges <- setNames(character(inputFiles$n), fileHandler$filenames)
      fileHandler$notes <- setNames(character(inputFiles$n), fileHandler$filenames)

      if (file.exists(summary_filepath)) {

        summary_table <- data.table::fread(summary_filepath)
        stopifnot(all(c("file", "tags", "notes") %in% colnames(summary_table)))
        files <- summary_table$file
        badges <- summary_table$tags
        notes <- summary_table$notes
        # Not super elegant but ensures we're only working with valid files
        for (i in seq_along(files)) {
          cur_file <- files[i]
          if (cur_file %in% fileHandler$filenames) {
            fileHandler$badges[cur_file] <- badges[i]
            fileHandler$notes[cur_file]  <- notes[i]
          }

        }
      }

      # Now that we've loaded the file, we can force update the plot
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

    return(list(outputDirInput = reactive(input$outputDirInput),
                inputDirInput  = reactive(input$inputDirInput)))
  })
}

