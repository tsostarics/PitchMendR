#' Color management module UI
#'
#' Renders the three color pickers on the settings pane
#'
#' @param id id for namespace
#'
#' @return UI elements for color management
#' @importFrom shiny tagList
colorUI <- function(id) {
  ns <- NS(id)
  # bslib::card(
  #   height = '88vh',
  #   fill = TRUE,
  #   title = "Color Settings",
  #   "Override default color settings below:",
  tagList(
    tags$span(title = "Select color of drawn lines",
              colourpicker::colourInput(
                inputId = ns("lineColor"),
                label = "Line color",
                value = "#10EBFF",
                showColour = "both",
                palette = "square"
              )),
    tags$span(title = "Select color of points where value is TRUE",
              colourpicker::colourInput(
                inputId = ns("keepTrueColor"),
                showColour = "both",
                palette = "square",
                label = "Color for points to KEEP/are not flagged",
                value = "#fdae61"
              )),
    tags$span(title = "Select color of points where value is FALSE",
              colourpicker::colourInput(
                inputId = ns("keepFalseColor"),
                showColour = "both",
                palette = "square",
                label = "Color for points to REMOVE/are flagged",
                value = "#df4461"
              ))
  )
}

#' Color management module server
#'
#' This module server takes care of the following functionality:
#'
#'  - Manage the color pickers on the settings page
#'  - Set the color pickers and plot settings when the user changes the theme
#'  - Set the correct color for the load files button
#'
#' @param id module id
#' @param plotSettings_ref Reference to the plot settings reactiveValues
#' @param updatePlot_reactive A reactive function that updates the plot
#' @param dark_mode_in A reactive function that returns the current theme,
#' should be a reactive wrapper around the dark mode input
#'
#' @return A module server function
#' @importFrom shiny NS
#' @importFrom ggplot2 element_line element_text element_rect aes
colorServer <- function(id,
                        plotSettings_ref,
                        updatePlot_reactive,
                        dark_mode_in,
                        fileHandler,
                        file_db) {
  moduleServer(id, function(input, output, session) {
    # Update the color pickers only when the user leaves the input, if we use
    # observeEvent then the plot will re-render when using updateColourInput
    # when changing the theme, which we don't want
    shinyjs::onevent(event = "click", id = ('lineColor'), {
      message("Changing line color")
      plotSettings_ref$setColors[1] <- input$lineColor
    })

    shinyjs::onevent(event = "click", id = ('keepTrueColor'), {
      message("Changing true color")
      plotSettings_ref$setColors[3] <- input$keepTrueColor
    })

    shinyjs::onevent(event = "click", id = ('keepFalseColor'), {
      message("Changing false color")
      plotSettings_ref$setColors[2] <- input$keepFalseColor
    })

    set_theme_colors <- reactive({
      # req(file_db)
      if (dark_mode_in() == "dark") {
        # Since we don't have any observers on the color pickers, this does two
        # things: changes the color picker values to a new color, which does not
        # cause the plot to re-render; then sets all 3 colors at once for the
        # plot settings, which causes the plot to re-render ONLY once.

        colourpicker::updateColourInput(session, "lineColor", value = "#10EBFF")
        colourpicker::updateColourInput(session, "keepTrueColor", value = "#fdae61")
        colourpicker::updateColourInput(session, "keepFalseColor", value = "#df4461")
        plotSettings_ref$setColors <- c("#10EBFF", "#fdae61",  "#df4461")
      } else{
        colourpicker::updateColourInput(session, "lineColor", value = "blue")
        colourpicker::updateColourInput(session, "keepTrueColor", value = "#111320")
        colourpicker::updateColourInput(session, "keepFalseColor", value = "#df4461")
        plotSettings_ref$setColors <- c("blue", "#111320",  "#df4461")
      }
      # updatePlot_reactive()
#
      print(plotSettings_ref$setColors)
    })

    # shiny::observeEvent(dark_mode_in(), {
    #   message("theme")
    #   # updateLoadFileColors()
    #   set_theme_colors()
      # browser()
#
#       if (is.null(fileHandler$indices))
#         return(NULL)
#
#       plotly::plotlyProxy("pulsePlot", session) |>
#         plotly::plotlyProxyInvoke("restyle",list(line.color = input$lineColor))#,
#                                   # seq(1,length(fileHandler$indices), 2))
    # })

    # change_colors <- reactive({
    #   req(input$lineColor,
    #       input$keepTrueColor,
    #       input$keepFalseColor,
    #       fileHandler,
    #       file_db)
    #
    #   plotly::plotlyProxy("pulsePlot", session) |>
    #     plotly::plotlyProxyInvoke("restyle",
    #                               list(color = input$lineColor),
    #                               seq(1,length(file_db$indices), 2)
    #                               which(file_db$indices %in% files_to_add))
    # })

    return(list(lineColor = reactive(input$lineColor),
                trueColor = reactive(input$keepTrueColor),
                falseColor = reactive(input$keepFalseColor),
                set_theme_colors = set_theme_colors))
  })}
