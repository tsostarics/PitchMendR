praatUI_input <- function(id) {
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
                             value = "./Praat.exe",
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
                             value = "./audio",
                             width = "100%"
                           ),
                           shiny::textInput(
                             inputId = ns("textgridDirInput"),
                             label = "TextGrid Directory",
                             value = "./audio",
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

praatServer <- function(id, loadedFile, fileHandler, filenameColumnInput) {
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
      if (!is.null(fileHandler$isPlotted) & !is.null(input$audioDirInput) & !is.null(input$fileNameGlue)) {

        # Use the columns of the loaded data and the provided glue string to
        # send the files currently displayed in the editor to Praat
        files_to_open <- unique(glue::glue_data_safe(loadedFile$data[loadedFile$data[[filenameColumnInput()]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                                              input$fileNameGlue))
        open_paths <- file.path(input$audioDirInput, files_to_open)

        if (!is.null(input$textgridDirInput))
          open_paths <- c(open_paths, file.path(input$audioDirInput, gsub(".wav$", ".TextGrid$", files_to_open)))

        systemcall <- paste(input$pathToPraat,
                            "--new-open --hide-picture",
                            paste0(files_to_open[file.exists(files_to_open)], collapse = " "),
                            sep = " ")
        message(systemcall)
        base::system(systemcall, wait = FALSE)
      }
    }
    )

    return(list(audioDirectory= reactive(input$audioDirInput),
                glueString= reactive(input$fileNameGlue)))
  })
}
