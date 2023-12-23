annotationUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::tabsetPanel(type = "hidden",
                         id = ns("switchBadges"),
                         shiny::tabPanelBody(value = "showBadges",
                                             shinyWidgets::checkboxGroupButtons(
                                               inputId = ns("badgeInput"),
                                               label = NULL,
                                               disabled = TRUE,
                                               size = "sm",
                                               status = "default ", # Trailing space prevents btn-default from being changed to btn-primary
                                               individual = FALSE,
                                               choices = c("Unusable", "Needs Attention", "Good Example"),
                                               justified = TRUE)),
                         shiny::tabPanelBody(value = "hideBadges", NULL))),
    shiny::fluidRow(
      shiny::tabsetPanel(type = "hidden",
                         id = ns("switchNotepad"),
                         shiny::tabPanelBody(value = "showNotepad",
                                             shiny::textAreaInput(
                                               inputId = ns("notepadInput"),
                                               label = NULL,
                                               width = "100%",
                                               height = "20px",
                                               resize = "both"
                                             )),
                         shiny::tabPanelBody(value = "hideNotepad", NULL)))
  )
}

annotationServer <- function(id, loadedFile, fileHandler, updatePlot,
                             input_filename,
                             input_noteToggle,
                             input_badgeToggle) {
  moduleServer(id, function(input, output, session) {
    badges_to_string <- function(badges){
      paste0(badges, collapse = "+")
    }

    string_to_badges <- function(string){
      if (is.na(string))
        return(c())
      strsplit(string, split = "\\+")[[1]]
    }

    updateBadges <- reactive({
      shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", selected = c())
      if (is.null(loadedFile$data)) {
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", disabled = TRUE)
        return(NULL)
      }

      if (sum(fileHandler$isPlotted) != 1) {
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", disabled = TRUE)
      } else {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        current_badges <- string_to_badges(loadedFile$data[loadedFile$data[[input_filename()]] == current_file, 'tags'][1][[1]])
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", disabled = FALSE, selected = current_badges)

      }
    })

    saveBadges <- reactive({
      if (sum(fileHandler$isPlotted) == 1) {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]

        loadedFile$data[loadedFile$data[[input_filename()]] == current_file, tags := badges_to_string(input$badgeInput)]
      }
    })

    updateNotes <- reactive({
      shiny::updateTextAreaInput(session, "notepadInput", value = "")
      if (is.null(loadedFile$data)) {
        shinyjs::disable("notepadInput")
        return(NULL)
      }

      if (sum(fileHandler$isPlotted) != 1) {
        shinyjs::disable("notepadInput")
      } else {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        current_note <- loadedFile$data[loadedFile$data[[input_filename()]] == current_file, 'notes'][1][[1]]
        message(current_note)
        shinyjs::enable("notepadInput")
        shiny::updateTextAreaInput(session, "notepadInput", value = current_note)

      }
    })

    saveNotes <- reactive({
      if (sum(fileHandler$isPlotted) == 1) {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]

        loadedFile$data[loadedFile$data[[input_filename()]] == current_file, notes := input$notepadInput]
      }
    })

    shiny::observeEvent(input_noteToggle(), {
      message("note toggle")
      if (is.null(loadedFile$data)) {
        shinyjs::disable("notepadInput")
        return(NULL)
      }

      if (input_noteToggle()) {
        if (!"notes" %in% colnames(loadedFile$data))
          loadedFile$data[, notes := ""]
      }

      if (input_noteToggle()){
        updateTabsetPanel(inputId = "switchNotepad", selected = "showNotepad")
      } else {
        updateTabsetPanel(inputId = "switchNotepad", selected = "hideNotepad")
      }
    })

    shiny::observeEvent(input_badgeToggle(), {
      if (is.null(loadedFile$data))
        return(NULL)

      if (input_badgeToggle()) {
        if (!"tags" %in% colnames(loadedFile$data))
          loadedFile$data[, tags := ""]
      }

      if (input_badgeToggle()){
        updateTabsetPanel(inputId = "switchBadges", selected = "showBadges")
      } else {
        updateTabsetPanel(inputId = "switchBadges", selected = "hideBadges")
      }
    })

    return(list(updateNotes = updateNotes,
                updateBadges = updateBadges,
                saveNotes = saveNotes,
                saveBadges = saveBadges))

  }
  )
}
