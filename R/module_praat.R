praatUI_input <- function(id,
                          praat_path = "./Praat.exe",
                          audio_directory = "./audio",
                          textgrid_directory = "./audio") {
  ns <- NS(id)

  tagList(
    "The following are only used to send files to Praat or play audio files from the editor.",
  bslib::accordion(
    id = ns("praatAccordion"),
    open = FALSE,
    width = "100%",
    bslib::accordion_panel(title = "Audio file options",
                           icon = icon('file-audio'),

                           shiny::textInput(
                             inputId = ns("pathToPraat"),
                             label = "Praat Path (relative to working directory)",
                             value = praat_path,
                             width = "100%",
                           ),
                           span(style = "display:inline-block;",
                                "Glue string to match file names to directory",
                                id = ns("glueQuestion"),
                                span(style = "cursor:pointer;", shiny::icon("circle-question"))),
                           shiny::textInput(
                             inputId = ns("fileNameGlue"),
                             label = NULL,
                             value = "{Speaker}_{Filename}.wav",
                             width = "100%",
                           ),
                           shiny::textInput(
                             inputId = ns("audioDirInput"),
                             label  = "Audio Directory",
                             value = audio_directory,
                             width = "100%"
                           ),
                           shiny::textInput(
                             inputId = ns("textgridDirInput"),
                             label = "TextGrid Directory",
                             value = textgrid_directory,
                             width = "100%"
                           )
    )
  )
  )

}

praatUI_button <- function(id) {
  ns <- NS(id)

  shiny::actionButton(
    inputId = ns("sendToPraatButton"),
    title = "Open the currently visible files in a new Praat window",
    label = "Open in Praat",
  )
}

praatServer <- function(id, loadedFile, fileHandler, filenameColumnInput, pitchRangeInput, saveData) {
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


    # When the user clicks the Send to Praat button, all files currently displayed
    # in the editor will be sent to Praat
    shiny::observeEvent(input$sendToPraatButton, {
      message("Send to Praat Pressed")
      if (is.null(loadedFile$data))
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

        pitch_range <- pitchRangeInput()

        # Can't set the pitch floor at 0
        if (!where_not_zero(pitch_range[1L]))
          ptich_range[1L] <- 40

        pitch_range <- paste(pitch_range, collapse = ", ")

        tg_lines <- c(
          "editorName$ = selected$()",
          'View & Edit'
        )

        if (length(tg_paths) > 0) {
          tg_lines <- c(
            paste0('tgObj = Read from file: "', tg_paths[1], '"'),
            "editorName$ = selected$()",
            'plusObject: obj',
            'View & Edit'
          )
        }

        script_lines <- c(
          paste0('obj = Read from file: "', audio_paths[1], '"'),
          tg_lines,
          "editor: editorName$",
          paste0("Pitch settings: ", pitch_range, ', "Hertz", "autocorrelation", "automatic"'),
          "endeditor"
        )

        # If we're loading more than one file, then we don't want to load the
        # first file twice. If we're only loading one file, then we don't need
        # to read anything more.
        read_file_lines <- ""
        if (length(audio_paths) > 1) {
          read_file_lines <- paste0('Read from file: \"', audio_paths[-1], '"')
        }

        # Run a self-deleting script that will open all the files in Praat
        run_temp_script(c(script_lines, read_file_lines), input$pathToPraat)

      }
    }
    )

    return(list(audioDirectory= reactive(input$audioDirInput),
                glueString= reactive(input$fileNameGlue),
                praatPath = reactive(input$pathToPraat)))
  })
}
