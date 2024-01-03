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
                value = "blue",
                showColour = "both",
                palette = "square"
              )),
    tags$span(title = "Select color of points where value is TRUE",
              colourpicker::colourInput(
                inputId = ns("keepTrueColor"),
                showColour = "both",
                palette = "square",
                label = "Color for points to KEEP/are not flagged",
                value = "black"
              )),
    tags$span(title = "Select color of points where value is FALSE",
              colourpicker::colourInput(
                inputId = ns("keepFalseColor"),
                showColour = "both",
                palette = "square",
                label = "Color for points to REMOVE/are flagged",
                value = "grey60"
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
colorServer <- function(id,
                        plotSettings_ref,
                        updatePlot_reactive,
                        dark_mode_in) {
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
      if (dark_mode_in() == "dark") {
        # Since we don't have any observers on the color pickers, this does two
        # things: changes the color picker values to a new color, which does not
        # cause the plot to re-render; then sets all 3 colors at once for the
        # plot settings, which causes the plot to re-render ONLY once.

        colourpicker::updateColourInput(session, "lineColor", value = "#10EBFF")
        colourpicker::updateColourInput(session, "keepTrueColor", value = "#fdae61")
        colourpicker::updateColourInput(session, "keepFalseColor", value = "#df4461")
        plotSettings_ref$setColors <- c("#10EBFF", "#df4461", "#fdae61")

        plotSettings_ref$themeColors <-
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "#1d1f21"),
            panel.grid.major = ggplot2::element_line(colour = "grey20"),
            panel.grid.minor = ggplot2::element_line(colour = "grey20"),
            axis.line = ggplot2::element_line(colour = "grey20"),
            axis.text.x = ggplot2::element_text(colour = "white"),
            axis.text.y = ggplot2::element_text(colour = "white"),
            axis.title.x = ggplot2::element_text(colour = "white"),
            axis.title.y = ggplot2::element_text(colour = "white"),
            strip.background = ggplot2::element_rect(fill = "grey20"),
            strip.text = ggplot2::element_text(colour = "white"),
            legend.background = ggplot2::element_rect(fill = "grey20"),
            legend.text = ggplot2::element_text(colour = "white"),
            legend.title = ggplot2::element_text(colour = "white"),
            plot.title = ggplot2::element_text(colour = "white"),
            plot.subtitle = ggplot2::element_text(colour = "white"),
            plot.caption = ggplot2::element_text(colour = "white"),
            plot.background = ggplot2::element_rect(fill = "#1d1f21")
          )
      } else{
        colourpicker::updateColourInput(session, "lineColor", value = "blue")
        colourpicker::updateColourInput(session, "keepTrueColor", value = "#111320")
        colourpicker::updateColourInput(session, "keepFalseColor", value = "#df4461")
        plotSettings_ref$setColors <- c("blue", "#df4461",  "#111320")

        plotSettings_ref$themeColors <-
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "white"),
            panel.grid.major = ggplot2::element_line(colour = "grey80"),
            panel.grid.minor = ggplot2::element_line(colour = "grey80"),
            axis.line = ggplot2::element_line(colour = "grey80"),
            axis.text.x = ggplot2::element_text(colour = "black"),
            axis.text.y = ggplot2::element_text(colour = "black"),
            axis.title.x = ggplot2::element_text(colour = "black"),
            axis.title.y = ggplot2::element_text(colour = "black"),
            strip.background = ggplot2::element_rect(fill = "grey80"),
            strip.text = ggplot2::element_text(colour = "black"),
            legend.background = ggplot2::element_rect(fill = "white"),
            legend.text = ggplot2::element_text(colour = "black"),
            legend.title = ggplot2::element_text(colour = "black"),
            plot.title = ggplot2::element_text(colour = "black"),
            plot.subtitle = ggplot2::element_text(colour = "black"),
            plot.caption = ggplot2::element_text(colour = "black"),
            plot.background = ggplot2::element_rect(fill = "white")
          )
      }
      updatePlot_reactive()
    })

    shiny::observeEvent(dark_mode_in(), {
      message("theme")
      # updateLoadFileColors()
      set_theme_colors()
    })
  })}
