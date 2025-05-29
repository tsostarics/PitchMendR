praatUI_input <- function(id,
                          praat_path = "./Praat.exe",
                          audio_directory = "./audio",
                          textgrid_directory = "./audio") {
  ns <- NS(id)

  tagList(
    singleton(tags$span(style = "display:inline-block",
                        "The following are only used to send files to Praat or play audio files from the editor.",
                        tags$span(id = ns("praatQuestion"),
                                  style = "cursor:pointer",
                                  shiny::icon("circle-question")))),
    bslib::accordion(
      id = ns("praatAccordion"),
      open = FALSE,
      width = "100%",
      bslib::accordion_panel(title = "Audio file options",
                             icon = icon('file-audio'),
                             tags$span(title = "Enter path to directory containing Praat",
                                       shiny::textInput(
                                         inputId = ns("pathToPraat"),
                                         label = "Praat Path (relative to working directory)",
                                         value = praat_path,
                                         width = "100%",
                                       )),
                             tags$span(title = "Enter string to be interpolated, click ? for more info",
                                       span(style = "display:inline-block;",
                                            "Glue string to match file names to directory",
                                            id = ns("glueQuestion"),
                                            span(style = "cursor:pointer;", shiny::icon("circle-question"))),
                                       shiny::textInput(
                                         inputId = ns("fileNameGlue"),
                                         label = NULL,
                                         value = "{Speaker}_{Filename}.wav",
                                         width = "100%",
                                       )),
                             shiny::verbatimTextOutput(ns("gluePathExample")),
                             tags$span(title = "Enter path to directory containing audio files",
                                       shiny::textInput(
                                         inputId = ns("audioDirInput"),
                                         label  = "Audio Directory",
                                         value = audio_directory,
                                         width = "100%"
                                       )),
                             tags$span(title = "Enter path to directory containing text grid files",
                                       shiny::textInput(
                                         inputId = ns("textgridDirInput"),
                                         label = "TextGrid Directory",
                                         value = textgrid_directory,
                                         width = "100%"
                                       ))
      )
    )
  )

}

praatUI_button <- function(id) {
  ns <- NS(id)

  span(style = "display:inline-block",
       # HTML(
       shiny::actionButton(
         inputId = ns("sendToPraatButton"),
         title = "Open the currently visible files in a new Praat window",
         label = "Open in Praat",
         class = "smallPlayButton",
         style = "padding-left:2px;padding-right:2px"
       ),
       shiny::actionButton(
         inputId = ns("clearPraatButton"),
         label = NULL,
         title = "Click to remove all open Praat objects",
         icon = shiny::icon("file-circle-minus", style = "font-size:1.5em;"),
         class = "showStopButton"
       )
  )
}

praatServer <- function(id, loadedFile, fileHandler, filenameColumnInput, pitchRangeInput, saveData,
                        navbarInput, brushedArea) {
  moduleServer(id, function(input, output, session) {

    shinyjs::onclick(id = "glueQuestion", {
      shinyWidgets::show_alert(
        title = "Glue strings",
        type = 'info',
        html = TRUE,
        text = tags$div(class = "manual-code",
                        style = css(`text-align` = "left"),
                        shiny::markdown(
                          mds = c(
                            "The [glue](https://glue.tidyverse.org/) package provides a convenient way to interpolate strings, similar to Python's f-strings.",
                            "Expressions in {curly braces} will be evaluated and the result will be inserted into the string.",
                            "Here, you can only work with the column names in your data.",
                            "",
                            "Let's say your speaker IDs and file names are in columns named
          `Speaker` and `Filename` respectively, with example values `spkr01` and `11_rise.wav`.
          The string `audio/Speaker_{Speaker}_{Filename}` will then become
          `audio/Speaker_spkr01_11_rise.wav`"
                          ))))

    })

    shiny::observeEvent(input$clearPraatButton, {
      message("Clear praat objects pressed")
      closePraatFiles()()
    })

    glueExample <- reactive({
      if (is.null(loadedFile$data) || is.null(input$fileNameGlue) || is.null(input$audioDirInput))
        return(NULL)

      example_file <- glue::glue_data_safe(loadedFile$data[1,], input$fileNameGlue)
      example_exists <- file.exists(file.path(input$audioDirInput, example_file))
      output_text <- paste0("Example file:\n", example_file)
      if (example_exists) {
        output_text <- paste0(output_text, "\nFile found!")
      } else {
        output_text <- paste0(output_text, "\nFile not found in audio directory!")
      }

      output_text

    })

    shiny::observeEvent(debounce(input$fileNameGlue, 200),{

      output$gluePathExample <- shiny::renderText(glueExample())
    })

    # When the user clicks the Send to Praat button, all files currently displayed
    # in the editor will be sent to Praat
    shiny::observeEvent(input$sendToPraatButton, {
      message("Send to Praat Pressed")
      sendToPraat()()
    }
    )


    shinyjs::onclick(id = "praatQuestion", {
      shinyWidgets::show_alert(

        title = "How is Praat used?",
        type = 'info',
        width = "35em",
        html = TRUE,
        text =
          tags$div(style = css(`text-align` = 'left'),
                   shiny::markdown(c("Temporary Praat scripts are created in the current working directory and contain commands to open or close files.",
                              "These scripts will automatically delete themselves once they are complete.",
                              "The app will be temporarily suspended while the Praat script runs, but will reactivate once the script completes.",
                              "If the script throws an error, the app may remain suspended and the temporary file will not automatically delete.",
                              "In this case, manually close Praat and delete the temporary script.",
                              "",
                              "If Praat is not currently running, a new Praat process will be launched.",
                              "The new Praat process will be a child process of the app, and so will be closed when the app is closed."))
          )
      )
    })

    closePraatFiles <- reactive({
      function(praatPath = input$pathToPraat) {
        # If there aren't any objects available the praat will throw an error,
        # so we make a small object at the start just in case
        delete_script_lines <-
          c(
            'Create Sound from formula: "sineWithNoise", 1, 0, 0.01, 400, "0"',
            "select all",
            "Remove")
        run_temp_script(delete_script_lines, praatPath)
      }
    })

    sendToPraat <- reactive({
      function(praatPath = input$pathToPraat) {
        if (is.null(loadedFile$data) | navbarInput() != "Editor")
          return(NULL)

        saveData()

        if (!is.null(fileHandler$isPlotted) & !is.null(input$audioDirInput) & !is.null(input$fileNameGlue)) {
          # Use the columns of the loaded data and the provided glue string to
          # send the files currently displayed in the editor to Praat
          files_to_open <- unique(glue::glue_data_safe(loadedFile$data[loadedFile$data[[filenameColumnInput()]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                                                       input$fileNameGlue))
          audio_paths <- file.path(input$audioDirInput, files_to_open)
          audio_paths <- audio_paths[file.exists(audio_paths)]
          tg_paths <- c(NULL)
          if (!is.null(input$textgridDirInput)){
            tg_paths <- file.path(input$textgridDirInput, gsub(".wav$", ".TextGrid", files_to_open))
            tg_paths <- tg_paths[file.exists(tg_paths)]
          }


          if (length(audio_paths) == 0){
            message("No files found")
            return(NULL)
          }


          # Set up a temporary script that will read in all the files
          temp_script <- tempfile(tmpdir = getwd(), fileext = ".praat")

          # Handle the pitch range options
          pitch_range <- pitchRangeInput()
          # Can't set the pitch floor at 0
          if (!where_not_zero(pitch_range[1L]))
            ptich_range[1L] <- 40

          pitch_range <- paste(pitch_range, collapse = ", ")

          # Get the selected points, if any
          time_range_values <- unlist(brushedArea()[c('xmin', 'xmax')])
          select_time_lines <- ""
          if (!is.null(time_range_values)) {
            time_range_values <- paste(time_range_values, collapse = ", ")
            select_time_lines <- paste0("Select: ", time_range_values)
          }
          # Ensure we have an editor even if we don't open any TextGrids
          tg_lines <- c(
            "if obj <> undefined",
            "editorName$ = selected$()",
            "fileOpened = 1",
            'View & Edit',
            "endif"
          )

          if (length(tg_paths) > 0) {
            tg_lines <- c(
              paste0('tgObj = nocheck Read from file: "', tg_paths[1], '"'),
              "if tgObj <> undefined",
              "editorName$ = selected$()",
              "if obj <> undefined",
              'plusObject: obj',
              "endif",
              "fileOpened = 1",
              'View & Edit',
              "endif"
            )
          }

          script_lines <- c(
            paste0('obj = nocheck Read from file: "', audio_paths[1], '"'),
            "fileOpened = 0",
            tg_lines,
            "if fileOpened = 1",
            'versionPraat$ = left$(praatVersion$, (rindex(praatVersion$, ".")-1));',
            "versionPraat = 'versionPraat$'",
            # In newer versions of praat, the pitch settings command is a bit
            # different. If the older styled command is run after removing an
            # object, then praat will crash. This avoids the issue, but needs
            # to be fixed in Praat itself.
            "editor: editorName$",
            "if versionPraat < 6.4",
            paste0("Pitch settings: ", pitch_range, ', "Hertz", "autocorrelation", "automatic"'),
            "else",
            paste0("Pitch settings: ", pitch_range, ', "Hertz", "filtered autocorrelation", "automatic", 0.0, 0.0'),
            "endif",
            select_time_lines,
            "endeditor",
            "endif"
          )

          # If we're loading more than one file, then we don't want to load the
          # first file twice. If we're only loading one file, then we don't need
          # to read anything more.
          read_audio_lines <- ""
          if (length(audio_paths) > 1) {
            read_audio_lines <-
              paste0('nocheck Read from file: \"', audio_paths[-1], '"')
          }

          read_tg_lines <- ""
          if (length(tg_paths) > 1) {
            read_tg_lines <-
              paste0('nocheck Read from file: \"', tg_paths[-1], '"')
          }

          # Usually true if the directories are set up correctly, but we check
          # just to be absolutely certain to avoid indexing errors. This allows
          # corresponding audio and textgrid files to be opened one after the
          # other. I.e., opens files in the object pane in the order on the left.
          #                           as opposed to:
          #         Sound file1                      Sound file1
          #         TextGrid file1                   Sound file2
          #         Sound file2                      TextGrid file1
          #         TextGrid file 2                  TextGrid file 2

          if (length(read_audio_lines) == length(read_tg_lines)){
            read_file_lines <- unlist(lapply(seq_along(read_audio_lines),
                                             \(i) c(read_audio_lines[i],
                                                    read_tg_lines[i])))
          } else {
            read_file_lines <- c(read_audio_lines, read_tg_lines)
          }

          # Run a self-deleting script that will open all the files in Praat
          run_temp_script(c(script_lines,read_file_lines), input$pathToPraat)

        }
      }
    })

    return(list(audioDirectory= reactive(input$audioDirInput),
                glueString= reactive(input$fileNameGlue),
                closePraatFiles = closePraatFiles,
                sendToPraat = sendToPraat))
  })
}
