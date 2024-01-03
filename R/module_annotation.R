annotationUI <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::tabsetPanel(type = "hidden",
                         id = ns("switchBadges"),

                         shiny::tabPanelBody(value = "showBadges",
                                             tags$span(title = "Click to add fast annotations to displayed file",
                                             shinyWidgets::checkboxGroupButtons(
                                               inputId = ns("badgeInput"),
                                               label = NULL,
                                               disabled = TRUE,
                                               size = "sm",
                                               status = "default ", # Trailing space prevents btn-default from being changed to btn-primary
                                               individual = FALSE,
                                               choices = c("Unusable", "Needs Attention", "Good Example"),
                                               justified = TRUE))),
                         shiny::tabPanelBody(value = "hideBadges", NULL))),
    shiny::fluidRow(
      shiny::tabsetPanel(type = "hidden",
                         id = ns("switchNotepad"),
                         tags$span(title = "Enter annotations for displayed file",
                         shiny::tabPanelBody(value = "showNotepad",
                                             shiny::textAreaInput(
                                               inputId = ns("notepadInput"),
                                               label = NULL,
                                               width = "100%",
                                               height = "20px",
                                               resize = "both"
                                             ))),
                         shiny::tabPanelBody(value = "hideNotepad", NULL)))
  )
}

annotationServer <- function(id, loadedFile, fileHandler, updatePlot,
                             input_filename,
                             input_noteToggle,
                             input_badgeToggle,
                             nPlotted) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    badges_to_string <- function(badges){
      paste0(badges, collapse = "+")
    }

    string_to_badges <- function(string){
      if (length(string) == 0 || is.na(string))
        return(character(0))
      strsplit(string, split = "\\+")[[1]]
    }

    updateBadges <- reactive({

      if (is.null(loadedFile$data)) {
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", disabled = TRUE)
        return(NULL)
      }

      if (!nPlotted$is_one) {
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", selected = character(0), disabled = TRUE)
      } else {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        current_badges <- string_to_badges(fileHandler$badges[current_file])
        shinyWidgets::updateCheckboxGroupButtons(session, "badgeInput", disabled = FALSE, selected = current_badges)
      }
    })

    saveBadges <- reactive({
      if (nPlotted$is_one) {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        fileHandler$badges[current_file] <- badges_to_string(input$badgeInput)
      }
    })

    updateNotes <- reactive({
      # shiny::updateTextAreaInput(session, "notepadInput", value = "")
      if (is.null(loadedFile$data)) {
        shinyjs::disable("notepadInput")
        return(NULL)
      }

      if (sum(fileHandler$isPlotted) != 1) {
        shiny::updateTextAreaInput(session, "notepadInput", value = character(0))
        shinyjs::disable("notepadInput")
      } else {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        current_note <- fileHandler$notes[current_file][[1]]
        if (is.na(current_note))
          current_note <- character(0)

        shinyjs::enable("notepadInput")
        shiny::updateTextAreaInput(session, "notepadInput", value = current_note)

      }
    })

    saveNotes <- reactive({
      if (sum(fileHandler$isPlotted) == 1) {
        current_file <- fileHandler$filenames[fileHandler$isPlotted]
        fileHandler$notes[current_file] <- input$notepadInput
        # loadedFile$data[loadedFile$data[[input_filename()]] == current_file, notes := input$notepadInput]
      }
    })

    shiny::observeEvent(input_noteToggle(),ignoreInit = TRUE, {

      if (!is.null(loadedFile$data)) {
        if (input_noteToggle()) {
          if (!"notes" %in% colnames(loadedFile$data))
            loadedFile$data[, notes := ""]
        }
      } else {
        shinyjs::disable("notepadInput")
      }

      if (input_noteToggle()){
        shiny::updateTabsetPanel(inputId = "switchNotepad", selected = "showNotepad")
      } else {
        shiny::updateTabsetPanel(inputId = "switchNotepad", selected = "hideNotepad")
      }

    })

    shiny::observeEvent(input_badgeToggle(), {

      if (!is.null(loadedFile$data)) {
        if (input_badgeToggle()) {
          if (!"tags" %in% colnames(loadedFile$data))
            loadedFile$data[, tags := ""]
        }
      }

      if (input_badgeToggle()){
        shiny::updateTabsetPanel(inputId = "switchBadges", selected = "showBadges")
      } else {
        shiny::updateTabsetPanel(inputId = "switchBadges", selected = "hideBadges")
      }
    })

    mergeAnnotations <- reactive({
      if (is.null(loadedFile$data)) {
        return(NULL)
      }

      if (input_badgeToggle()) {
        loadedFile$data[, tags := fileHandler$badges[loadedFile$data[[input_filename()]]]]
      }

      if (input_noteToggle()) {
        loadedFile$data[, notes := fileHandler$notes[loadedFile$data[[input_filename()]]]]
      }
    })

    return(list(updateNotes = updateNotes,
                updateBadges = updateBadges,
                saveNotes = saveNotes,
                saveBadges = saveBadges,
                mergeAnnotations = mergeAnnotations))

  }
  )
}
