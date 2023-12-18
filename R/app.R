#' Run pulse editor GUI
#'
#' Runs the shiny app
#'
#' @param ... not used
#'
#' @return Nothing
#' @export
#'
#' @importFrom shiny tags
openEditor <- function(...) {
  ui <- bslib::page_navbar(

    id = "navbar",
    title = "Pitch Editor",
    selected = "Setup",
    collapsible = TRUE,
    theme = bslib::bs_theme(),
    sidebar = bslib::sidebar(
      shinyjs::useShinyjs(), # Placed here to avoid a warning if placed above a tab
      title = "Tools",
      shiny::actionButton(
        inputId = "loadFileButton",
        label = "Load File",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
        shiny::icon("upload")
      ),
      shiny::actionButton(
        inputId = "refreshProgressButton",
        label = "Refresh Diagnostics"
      ),
      shiny::actionButton(
        inputId = "plotUneditedFilesButton",
        label = "Send Unedited Files to Editor"
      ),
      shiny::actionButton(
        inputId = "sendToPraatButton",
        label = "Open Files in Praat",
      ),
      shiny::actionButton(
        inputId = "undoTransformButton",
        label = "Undo Last Pulse Transformation"
      ),
      shiny::actionButton(
        inputId = "checkVisibleFilesButton",
        label = "Check off all visible files"
      ),
      shiny::uiOutput(outputId = "uneditedFileSelectUI"),
      shiny::uiOutput(outputId = "editedFileSelectUI")
    ),
    bslib::nav_item(
      bslib::input_dark_mode(id = "dark_mode", mode = "dark")
    ),
    bslib::nav_panel(
      # Remove the annoying borders between all the cards and make the spacing
      # a bit less overbearing
      tags$head(
    tags$style(shiny::HTML("
      .card {
        --bs-card-spacer-y: 0.5rem;  /* Adjust this value as needed */
        --bs-card-border-width: none;
      }
    "))
  ),
      title = "Editor",
      gridlayout::grid_container(
        id = "plotPanel",
        layout = c(
          "num_chicks pulsePlot       ",
          "num_chicks bottomButtonArea"
        ),
        row_sizes = c(
          "0.65fr",
          "0.35fr"
        ),
        col_sizes = c(
          "250px",
          "1fr"
        ),
        gap_size = "0px",
        gridlayout::grid_card(
          area = "num_chicks",
          bslib::card_header("Plot Settings"),
          bslib::card_body(
            shiny::textOutput(outputId = "workingFileOutput"),
            shiny::uiOutput(outputId = "sizeSliderUI"),
            shiny::uiOutput(outputId = "alphaSliderUI"),
            shiny::textInput(
              inputId = "filterRegex",
              label = "Plot files matching regex",
              value = "."
            ),
            shiny::actionButton(
              inputId = "plotMatchesButton",
              label = "Plot Matches"
            ),
            "Number of files plotted:",
            shiny::textOutput(outputId = "nFilesPlotted"),
            shiny::actionButton(
              inputId = "goToBrushButton",
              label = "Plot Brushed Files"
            ),
            shiny::verbatimTextOutput(outputId = "brushedFileNames")
          )
        ),
        gridlayout::grid_card_plot(area = "pulsePlot", brush = "plot_brush"),
        gridlayout::grid_card(
          area = "bottomButtonArea",
          bslib::card_body(
            gap = "0px",
            gridlayout::grid_container(
              layout = c(
                "buttonsTopLeft toggleButtonCard"
              ),
              gap_size = "0px",
              col_sizes = c(
                "0.5fr",
                "0.5fr"
              ),
              row_sizes = c(
                "1fr"
              ),
              gridlayout::grid_card(
                area = "buttonsTopLeft",
                bslib::card_body(
                  gap = "0px",
                  shiny::actionButton(
                    inputId = "showLineButton",
                    label = "Show Line"
                  ),
                  gridlayout::grid_container(
                    container_height = "80",
                    layout = c(
                      "prevButtonCard saveButtonCard nextButtonCard"
                    ),
                    gap_size = "0px",
                    col_sizes = c(
                      "0.34fr",
                      "0.32fr",
                      "0.34fr"
                    ),
                    row_sizes = c(
                      "1fr"
                    ),
                    gridlayout::grid_card(
                      area = "prevButtonCard",
                      bslib::card_body(
                        shiny::actionButton(inputId = "prevButton", label = "<")
                      )
                    ),
                    gridlayout::grid_card(
                      area = "nextButtonCard",
                      bslib::card_body(
                        shiny::actionButton(inputId = "nextButton", label = ">")
                      )
                    ),
                    gridlayout::grid_card(
                      area = "saveButtonCard",
                      bslib::card_body(
                        shiny::actionButton(
                          inputId = "saveButton",
                          shiny::uiOutput(outputId = "saveButtonLabel")
                        ),
                      )
                    )
                  )
                )
              ),
              gridlayout::grid_card(
                area = "toggleButtonCard",
                bslib::card_body(
                  gap = "0px",
                  shiny::actionButton(
                    inputId = "toggleButton",
                    label = "Toggle Pulses"
                  ),
                  gridlayout::grid_container(
                    container_height = "140",
                    layout = c(
                      "keepButtonCard removeButtonCard",
                      "halfButtonCard doubleButtonCard"
                    ),
                    row_sizes = c(
                      "0.5fr",
                      "0.5fr"
                    ),
                    col_sizes = c(
                      "0.5fr",
                      "0.5fr"
                    ),
                    gap_size = "0px",
                    gridlayout::grid_card(
                      area = "keepButtonCard",
                      bslib::card_body(
                        shiny::actionButton(inputId = "keepButton", label = "Keep")
                      )
                    ),
                    gridlayout::grid_card(
                      area = "removeButtonCard",
                      bslib::card_body(
                        shiny::actionButton(inputId = "removeButton", label = "Remove")
                      )
                    ),
                    gridlayout::grid_card(
                      area = "halfButtonCard",
                      bslib::card_body(
                        shiny::actionButton(
                          inputId = "halfButton",
                          label = "Halve Pulses"
                        )
                      )
                    ),
                    gridlayout::grid_card(
                      area = "doubleButtonCard",
                      bslib::card_body(
                        shiny::actionButton(
                          inputId = "doubleButton",
                          label = "Double Pulses"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Settings",
      gridlayout::grid_container(
        layout = c(
          "columnSettingsCard settingsPane2 selectionInstructionsCard"
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          "0.25fr",
          "0.25fr",
          "0.5fr"
        ),
        gap_size = "10px",
        gridlayout::grid_card(
          area = "columnSettingsCard",
          bslib::card_body(
            shiny::textInput(
              inputId = "filenameColumnInput",
              label = "Column name containing individual files",
              value = "Filename"
            ),
            shiny::textInput(
              inputId = "xValColumnInput",
              label = "X-value column name",
              value = "t_ms"
            ),
            shiny::textInput(
              inputId = "yValColumnInput",
              label = "Y-value column name",
              value = "f0"
            ),
            shiny::textInput(
              inputId = "selectionColumnInput",
              label = "Name of logical column to use for selection (will be added if it doesn't exist)",
              value = "keep_points",
              width = "100%"
            ),
            shiny::radioButtons(
              inputId = "useFlaggedColumnRadio",
              label = "Use column to color code pulses?",
              choices = list("Yes" = "TRUE", "No" = "FALSE"),
              width = "100%"
            ),
            shiny::textInput(
              inputId = "colorCodeColumnInput",
              label = "If Yes, Column to use for color coding. Default ggplot colors will be used.",
              value = "flagged_samples",
              width = "100%"
            ),
            shiny::radioButtons(
              inputId = "saveOptionButton",
              label = "Save on file navigation?",
              selected = "FALSE",
              choices = list("Yes" = "TRUE", "No" = "FALSE"),
              width = "100%"
            )
          )
        ),
        gridlayout::grid_card(
          area = "selectionInstructionsCard",
          bslib::card_body(
            shiny::markdown(
              mds = c(
                "## Selecting points",
                "",
                "Upon opening a new dataframe to annotate, a new column (`keep_points`) will be added to the dataframe and filled with `TRUE`.",
                "",
                "The pitch pulses for a specified file will be plotted in the main plotting region.
              Points that you want to keep (`keep_points=TRUE`) will be shown in closed circles, while points you want to remove (`keep_points=TRUE`) will be replaced with an open triangle.
              You can use the `[Show Line]` toggle button to plot a line through all the retained points.",
              "",
              "To remove a point, click and drag to select the desired points, then click the `[Remove]` button.
              To keep a point, click the `[Keep]` Button.
              You can click the `[Toggle Pulse]` button to flip the values of the selecteed points (all removed points will be switched to keep, all kept values will be switched to remove).
              ",
              "",
              "## Praat integration",
              "",
              "Using the `[Open Files in Praat]` button will open the files that are visible in the plot using the praat executable at the given path.
              This is accomplished via a `--new-open` system call.
              This can be useful when it's not clear based on just the extracted pitch contour whether particular pulses are tracking errors or not or if you just need to listen to the audio files."
              )
            )
          )
        ),
        gridlayout::grid_card(
          area = "settingsPane2",
          bslib::card_body(
            "Override default color settings below:",
            colourpicker::colourInput(
              inputId = "lineColor",
              label = "Line color",
              value = "blue",
              showColour = "both",
              palette = "square"
            ),
            colourpicker::colourInput(
              inputId = "keepTrueColor",
              showColour = "both",
              palette = "square",
              label = "Color for pulses to KEEP",
              value = "black"
            ),
            colourpicker::colourInput(
              inputId = "keepFalseColor",
              showColour = "both",
              palette = "square",
              label = "Color for pulses to REMOVE",
              value = "grey60"
            ),
            shiny::sliderInput(
              inputId = "plotPanelRatio",
              label = "% of Screen to use for plot panel (decrease for smaller screens)",
              min = .01,
              max = .99,
              value = .65,
              step = .05
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Progress",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(""),
        bslib::card_body(
          "Percentage of pulses removed from current file:",
          shiny::textOutput(outputId = "percentRemovedText"),
          "Number of files / total files found with any pulses removed:",
          shiny::textOutput(outputId = "nEditedFilesText"),
          "These files have received no edits:",
          shiny::verbatimTextOutput(outputId = "uneditedFilesText")
        )
      )
    ),
    bslib::nav_panel(
      tags$head(tags$style("body{overflow:hidden;}")),
      title = "Setup",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(""),
        bslib::card_body(
          tags$head(tags$style("body{overflow:hidden;}")),

          gridlayout::grid_container(container_height = "100%",
                                     tags$head(tags$style("body{overflow:hidden;}")),
                                     layout = c(
                                       "area0 area1"
                                     ),
                                     row_sizes = c(
                                       "1fr"
                                     ),
                                     col_sizes = c(
                                       "0.5fr",
                                       "0.5fr"
                                     ),
                                     gap_size = "10px",
                                     gridlayout::grid_card(area = "area0",
                                                           bslib::card_body(
                                                             "Current working directory:",
                                                             shiny::verbatimTextOutput(outputId = "cwd"),
                                                             shiny::textInput(
                                                               inputId = "inputDirInput",
                                                               label = "Input directory",
                                                               value = get_example_f0_data(".")
                                                             ),
                                                             shiny::textInput(
                                                               inputId = "outputDirInput",
                                                               label = "Output directory",
                                                               value = "."
                                                             ),
                                                             shiny::uiOutput(outputId = "availableFilesUI"),
                                                             shiny::textInput(
                                                               inputId = "pathToPraat",
                                                               label = "Praat Path (relative to app.R directory)",
                                                               value = "./Praat.exe"
                                                             ),
                                                             shiny::textInput(
                                                               inputId = "fileNameGlue",
                                                               label = "Glue string to match file names to directory",
                                                               value = "{Speaker}_{Filename}.wav"
                                                             ),
                                                             shiny::textInput(
                                                               inputId = "audioDirInput",
                                                               label = "Audio Directory",
                                                               value = "./mfa_input"
                                                             ),
                                                             shiny::textInput(
                                                               inputId = "textgridDirInput",
                                                               label = "Textgrid Directory",
                                                               value = "./mfa_input"
                                                             ),
                                                           )),
                                     gridlayout::grid_card(
                                       area = "area1",
                                       bslib::card_body(
                                         "If you have multiple .PitchTier files in the input directory, you can load them into a single dataframe here. This will save a new csv file into the input directory.",
                                         gridlayout::grid_container(container_height = "150",
                                                                    layout = c(
                                                                      "pitchTierPanelLeft pitchTierPanelRight"
                                                                    ),
                                                                    row_sizes = c(
                                                                      "1fr"
                                                                    ),
                                                                    col_sizes = c(
                                                                      "1fr",
                                                                      "1fr"
                                                                    ),
                                                                    gap_size = "0px",
                                                                    gridlayout::grid_card(
                                                                      area = "pitchTierPanelLeft",
                                                                      bslib::card_body(
                                                                        shiny::textInput(
                                                                          inputId = "speakerIDInput",
                                                                          label = "Speaker ID to add",
                                                                          value = ""
                                                                        )
                                                                      )
                                                                    ),
                                                                    gridlayout::grid_card(
                                                                      area = "pitchTierPanelRight",
                                                                      bslib::card_body(
                                                                        shiny::textInput(
                                                                          inputId = "pitchTierRegex",
                                                                          label = "Load files matching regex",
                                                                          value = ".+PitchTier$"
                                                                        )
                                                                      )
                                                                    )
                                         ),
                                         shiny::actionButton(
                                           inputId = "processPitchTiersButton",
                                           label = "Load PitchTiers as CSV"
                                         ),
                                         shiny::markdown(
                                           mds = c(
                                             "## Flagging samples",
                                             "",
                                             "Once you have loaded the files, you can use an automated sample-to-sample method to flag samples that are likely to be tracking errors.
              This method is based on identifying octave jumps between adjacent samples.
              Note that this method is not perfect and may flag samples that are not tracking errors and miss samples that are tracking errors.",
              "",
              "If the column `F0_semitones` already exists, it will be used to track errors.
              If not, this column will be added by computing semitones from the median pitch of all of the speaker's files.
              Check the settings tab for the column names used for the time, pitch, and filename values.",
              "",
              "If the column `flagged_samples` already exists, it will be overwritten."
                                           )
                                         ),
              shiny::actionButton(
                inputId = "flagSamplesButton",
                shiny::uiOutput(outputId = "flagSamplesButtonLabel")
              ),
                                       )
                                     )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {
    # Create a reactive value to store the selected points
    selectedPoints <- shiny::reactiveValues(data = NULL)
    plotSettings <- shiny::reactiveValues(data = NULL,
                                          themeColors = NULL)
    fileHandler <- shiny::reactiveValues(filenames = NULL,
                                         isPlotted = NULL,
                                         fileChecked = NULL)
    filesBrushed <- shiny::reactiveValues(filenames = NULL)
    uneditedFiles <- shiny::reactiveValues(filenames = NULL)
    lastTransformation <- shiny::reactiveValues(pulse_ids = NULL)
    loadedFile <- shiny::reactiveValues(data = NULL)
    transformedColumn <- shiny::reactiveValues(name = NULL)
    plotFlag <- shiny::reactiveValues(value = TRUE)

    shiny::observeEvent(input$dark_mode, {
      updateLoadFileColors()
      if (input$dark_mode == "dark") {
        colourpicker::updateColourInput(session, "lineColor", value = "#10EBFF")
        colourpicker::updateColourInput(session, "keepTrueColor", value = "#fdae61")
        colourpicker::updateColourInput(session, "keepFalseColor", value = "#df4461")
        plotSettings$themeColors <-
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

        plotSettings$themeColors <-
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
    })

    # input$dark_mode

    getBrushedPoints <- shiny::reactive({
      shiny::brushedPoints(loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                           input$plot_brush,
                           xvar = input$xValColumnInput,
                           yvar = transformedColumn$name)
    })

    output$pulsePlot <- shiny::renderPlot({
      # Color the points based on the color column
      # check for alphaSlider ensures that the plot is not rendered before the
      #   inputs have loaded their initial values
      if (!is.null(plotFlag$value) & !is.null(loadedFile$data) & !is.null(input$alphaSlider)) {

        plot_subset <- loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],]

        # If the user wants to use the flagged column, make sure it exists and is a factor
        if (input$useFlaggedColumnRadio == "TRUE" && input$colorCodeColumnInput %in% colnames(plot_subset)) {
          plot_subset[, (input$colorCodeColumnInput) := factor(get(input$colorCodeColumnInput))]
        }
        # Set up the main aesthetics for the plot
        p <-
          ggplot2::ggplot(plot_subset,
                          ggplot2::aes(x = !!rlang::sym(input$xValColumnInput),
                                       y = !!rlang::sym(transformedColumn$name),
                                       group = !!rlang::sym(input$filenameColumnInput)))

        # Add the line if the user wants it, this should go under the points
        if (plotSettings$showLine) {
          p <- p +
            ggplot2::geom_line(data =plot_subset[plot_subset$keep_pulse,], color = input$lineColor)
        }

        # Add the points, if the user wants to use the flagged column, use it to color the points
        # otherwise just use the keep_pulse column to redundantly code that information
        if (input$useFlaggedColumnRadio == "TRUE" && input$colorCodeColumnInput %in% colnames(plot_subset)) {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(input$colorCodeColumnInput), shape = keep_pulse),
                                size = input$sizeSlider,
                                alpha = input$alphaSlider)
        } else {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = keep_pulse, shape = keep_pulse),
                                size = input$sizeSlider,
                                alpha = input$alphaSlider)
        }

        # If the color column is binary, use the colors specified by the user,
        # otherwise just use the default colors
        if (input$useFlaggedColumnRadio == "FALSE" ||
            length(unique(plot_subset[[input$colorCodeColumnInput]])) == 2) {
          color_values <- c(input$keepFalseColor, input$keepTrueColor)
          if (input$useFlaggedColumnRadio == "TRUE")
            color_values <- rev(color_values)
          p <- p +
            ggplot2::scale_color_manual(values = color_values)
        }

        # Finish setting some of the theme options
        p <- p +
          ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 2)) +
          ggplot2::xlab(input$xValColumnInput) +
          ggplot2::ylab(input$yValColumnInput) +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::theme(legend.position = 'top') +
          ggplot2::theme(panel.grid = ggplot2::element_line(linewidth = .3)) +
          plotSettings$themeColors +
          # If a single file is shown, use that file as the title, otherwise use "Multiple Files"
          ggplot2::ggtitle(label = ifelse(sum(fileHandler$isPlotted) == 1,
                                          fileHandler$filenames[fileHandler$isPlotted],
                                          "Multiple Files"))

        p
      }})

    # When the user clicks the toggle button, toggle the logical keep_pulse value
    shiny::observeEvent(input$toggleButton, {
      message("Toggle Pressed")
      selectedPoints$data <- getBrushedPoints()

      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data$keep_pulse[vals_to_change] <- !loadedFile$data$keep_pulse[vals_to_change]
        # updatePlotSettingsData()
        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # When the user clicks the keep button, set the logical keep_pulse value to TRUE
    shiny::observeEvent(input$keepButton, {
      message("Keep Pressed")
      selectedPoints$data <- getBrushedPoints()

      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, keep_pulse := TRUE]
        # updatePlotSettingsData()
        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # When the user clicks the remove button, set the logical keep_pulse value to FALSE
    shiny::observeEvent(input$removeButton, {
      message("Remove Pressed")
      selectedPoints$data <- getBrushedPoints()

      # Toggle the color of the selected points
      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, keep_pulse := FALSE]
        # updatePlotSettingsData()

        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # Whenever the user toggles the dark mode, update the colors of the load file
    # button. It should be blue no matter what if no file is loaded, then fade
    # to an unselected gray color after a file is loaded.
    updateLoadFileColors <- function() {
      if (input$loadFileButton > 0){
        if (input$dark_mode == "dark"){
          shinyjs::runjs('document.getElementById("loadFileButton").style.backgroundColor = "#2a2c30";')
          shinyjs::runjs('document.getElementById("loadFileButton").style.color = "white";')
          shinyjs::runjs('document.getElementById("loadFileButton").style.borderColor = "black";')
          # textColor$value <- "white"
        } else {
          shinyjs::runjs('document.getElementById("loadFileButton").style.backgroundColor = "#ececec";')
          shinyjs::runjs('document.getElementById("loadFileButton").style.color = "black";')
          shinyjs::runjs('document.getElementById("loadFileButton").style.borderColor = "black";')
          # textColor$value <- "black"
        }
      }
    }

    # When the user clicks the load file button, load the file and initialize
    # different columns and reactive values
    # Install and load the data.table package


    shiny::observeEvent(input$loadFileButton, {
      message("Load File Pressed")

      if (!dir.exists(input$inputDirInput)){
        message("Input directory doesnt exist")
        return(NULL)
      }

      file_to_load <- file.path(input$inputDirInput, input$fileSelectBox)
      outFile <- file.path(input$outputDirInput, input$fileSelectBox)
      if (file.exists(outFile)) {
        message("Resuming progress from existing output file")
        file_to_load <- outFile
      }

      message(paste0("Loading file ", file_to_load))
      loadedFile$data <- data.table::fread(file_to_load)  # Use fread from data.table package

      # If the file doesn't contain the specified columns, return null and move
      # to the settings page
      if (!all(c(input$filenameColumnInput, input$xValColumnInput, input$yValColumnInput) %in% colnames(loadedFile$data))) {
        message("File doesn't contain the specified columns")
        loadedFile$data <- NULL
        shiny::updateNavbarPage(session, "navbar", "Settings")
        return(NULL)
      }

      updateLoadFileColors()

      data.table::setorderv(loadedFile$data, cols = c(input$filenameColumnInput, input$xValColumnInput))  # Use setorder from data.table package

      if (!file.exists(outFile))
        loadedFile$data[, keep_pulse := get(input$yValColumnInput) > 2]  # Use := operator from data.table package

      if (!"pulse_id" %in% colnames(loadedFile$data))
        loadedFile$data[, pulse_id := .I]  # Use .I from data.table package

      if (!"pulse_transform" %in% colnames(loadedFile$data)){
        loadedFile$data[, pulse_transform := 1.0]
      }

      transformedColumn$name <- paste(input$yValColumnInput, "transformed", sep = "_")
      loadedFile$data[, (transformedColumn$name) := pulse_transform * get(input$yValColumnInput)]  # Use get from data.table package

      fileHandler$filenames <- unique(loadedFile$data[[input$filenameColumnInput]])
      fileHandler$isPlotted <- rep(FALSE, length(fileHandler$filenames))
      fileHandler$isPlotted[1] <- TRUE

      if (!"file_checked" %in% colnames(loadedFile$data)) {
        loadedFile$data[, file_checked := FALSE]
        fileHandler$fileChecked <- rep(FALSE, length(fileHandler$filenames))
      } else {
        loaded_file_check <- loadedFile$data[, .(file_checked = ifelse(is.na(file_checked[1]), FALSE, file_checked[1])), by = c(input$filenameColumnInput)]

        file_checks <- loaded_file_check$file_checked
        names(file_checks) <- loaded_file_check[[input$filenameColumnInput]]
        fileHandler$fileChecked <- file_checks[fileHandler$filenames]
      }

      plotSettings$showLine <- FALSE

      output$workingFileOutput <- shiny::renderText({
        paste0("Working File:\n", input$fileSelectBox)
      })

      shiny::updateNavbarPage(session, "navbar", "Editor")
    })

    # Toggle whether the line should be shown or not
    shiny::observeEvent(input$showLineButton, {
      plotSettings$showLine <- !plotSettings$showLine
    })

    # When the user clicks the save button, save the data to the output directory
    shiny::observeEvent(input$saveButton, {
      message("Save Pressed")
      if (sum(fileHandler$isPlotted) == 1) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }

      if (!dir.exists(input$outputDirInput)){
        message("Output directory doesnt exist")
        saveIcon$value <- "triangle-exclamation"
        return(NULL)
      }

      saveData(file.path(input$outputDirInput, input$fileSelectBox))
    })

    # When the user clicks the next button, plot the next file alphabetically
    shiny::observeEvent(input$nextButton, {
      # Get the maximum index of the files that are currently plotted,
      # if we're already at the last file, wrap around to the first file
      current_max <- max(which(fileHandler$isPlotted))
      if (current_max > length(fileHandler$filenames))
        current_max <- 0

      # Check off the file that's currently plotted before we move to the next file
      if (sum(fileHandler$isPlotted) == 1) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }

      fileHandler$isPlotted[] <- FALSE
      fileHandler$isPlotted[current_max + 1] <- TRUE

      # If we have the save-on-next option enabled, save the data
      if (input$saveOptionButton == "TRUE") {
        saveData(file.path(input$outputDirInput, input$fileSelectBox))
      }

    })

    # When the user clicks the previous button, plot the previous file alphabetically
    shiny::observeEvent(input$prevButton, {
      # Get the minimum index of the files that are currently plotted,
      # if we're already at the first file, wrap around to the last file
      current_min <- min(which(fileHandler$isPlotted))
      if (current_min == 1)
        current_min <- length(fileHandler$filenames) + 1

      #$ Check off the file that's currently plotted before we move to the next file
      if (sum(fileHandler$isPlotted) == 1) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }

      fileHandler$isPlotted[] <- FALSE
      fileHandler$isPlotted[current_min - 1] <- TRUE

      if (input$saveOptionButton == "TRUE") {
        saveData(file.path(input$outputDirInput, input$fileSelectBox))
      }

    })

    # Display the number of files/contours that are currently plotted
    output$nFilesPlotted <- shiny::renderText({
      sum(fileHandler$isPlotted)
    })

    # When the user clicks the plot matches button, plot the files that match the
    # regex in the filter regex box
    shiny::observeEvent(input$plotMatchesButton, {
      message("Plot Matches Pressed")
      print(input$filterRegex)
      fileHandler$isPlotted <- grepl(input$filterRegex,fileHandler$filenames)

    })

    # Display the filenames of the selected points when the user makes a selection
    output$brushedFileNames <- shiny::renderText({
      if (!is.null(loadedFile$data)) {
        # With base graphics, need to tell it what the x and y variables are.
        # message("Brush")
        selectedPoints$data <- getBrushedPoints()
        filesBrushed$filenames <- unique(selectedPoints$data[[input$filenameColumnInput]])
        paste0("# Brushed: ", length(filesBrushed$filenames), "\n\n",
               paste(filesBrushed$filenames, collapse = "\n"))
      }
    })

    # When the user clicks the save button, save the data to the output directory
    saveData <- function(path) {
      if (is.null(data))
        return(NULL)

      # Update the file checked column before saving
      loadedFile$data[, file_checked := fileHandler$fileChecked[.SD[[input$filenameColumnInput]]]]
      if (!file.exists(path) || file.access(path, mode = 2) == 0) {
        saveIcon$value <- "spinner"
        data.table::fwrite(x = loadedFile$data, file = path)
        message(paste0("Wrote data to ", path))
        saveIcon$value <- "floppy-disk"
      }

    }

    saveIcon <- shiny::reactiveValues(value = "floppy-disk")

    # When the user clicks the save button, save the data to the output directory
    # When the user clicks the go to brushed button, plot only the files that the
    # user selected in the editor
    shiny::observeEvent(input$goToBrushButton, {
      brushed_regex <- paste0("(", filesBrushed$filenames, ")", collapse = "|")
      fileHandler$isPlotted <- grepl(brushed_regex,fileHandler$filenames)

      message(paste0("Plotting ", sum(fileHandler$isPlotted), " files from selection"))
    })

    # Refresh the diagnostics pane when the user clicks the refresh button
    shiny::observeEvent(input$refreshProgressButton,{
      filtered_data <- loadedFile$data[get(input$yValColumnInput) > 2]

      output$percentRemovedText <- shiny::renderText({
        paste0(round(sum(!filtered_data$keep_pulse) / nrow(filtered_data)*100, 2), "%")
      })

      output$nEditedFilesText <- shiny::renderText({
        n_edited <- loadedFile$data[, .(n_edited = sum(!keep_pulse)), by = c(input$filenameColumnInput)][n_edited > 0, .N]
        paste0(n_edited, " / ", length(unique(filtered_data[[input$filenameColumnInput]])))
      })

      output$uneditedFilesText <- shiny::renderText({
        uneditedFiles$filenames <-
          filtered_data[, .(n_edited = sum(!keep_pulse)), by = c(input$filenameColumnInput)][n_edited == 0, unique(.SD[[input$filenameColumnInput]])]

        paste(uneditedFiles$filenames, collapse = "\n")
      })

    })

    updatePlot <- shiny::reactive({
      plotFlag$value <- !plotFlag$value
    })


    # Multiply selected points by 2 (fixes halving errors)
    shiny::observeEvent(input$doubleButton, {
      message("Double Pressed")
      selectedPoints$data <-getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id


      loadedFile$data[vals_to_change, pulse_transform := pulse_transform * 2.0]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change
      updatePlot()
    })

    # Multiply selected points by 0.5 (fixes doubling errors)
    shiny::observeEvent(input$halfButton, {
      message("Halve Pressed")
      selectedPoints$data <-getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id


      loadedFile$data[, pulse_transform:= as.numeric(pulse_transform)][vals_to_change, pulse_transform := pulse_transform * 0.5]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]


      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change
      updatePlot()
    })

    # Undo the last transformation, resetting the transformation to 1
    shiny::observeEvent(input$undoTransformButton, {
      message("Undo Pressed")
      if (!is.null(lastTransformation$pulse_ids)) {
        # vals_to_change <- loadedFile$data$pulse_id %in% lastTransformation$pulse_ids
        # print(lastTransformation$pulse_ids)
        loadedFile$data[lastTransformation$pulse_ids, pulse_transform := 1.0]
        loadedFile$data[lastTransformation$pulse_ids, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

        lastTransformation$pulse_ids <- NULL
        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # When the user clicks the plot unedited files button, plot only the files
    # that the user has not edited. Must be done after the user has refreshed
    # the diagnostics pane
    shiny::observeEvent(input$plotUneditedFilesButton, {
      if (!is.null(uneditedFiles$filenames)) {
        fileHandler$isPlotted <- grepl(paste0("(", uneditedFiles$filenames, ")", collapse = "|"),fileHandler$filenames)
      }
    })

    # Renders the selectInput for all unedited files.
    # User can select a file from this box to plot it.
    output$uneditedFileSelectUI <- shiny::renderUI( {
      if (is.null(fileHandler$filenames))
        return(NULL)

      shiny::selectInput("uneditedFileSelectBox", "Unchecked files",
                         multiple = FALSE,
                         selectize = TRUE,
                         choices = fileHandler$filenames[!fileHandler$fileChecked])

    })

    # Renders the selectInput for all edited files.
    output$editedFileSelectUI <- shiny::renderUI({
      if (is.null(fileHandler$filenames))
        return(NULL)

      shiny::selectInput("editedFileSelectBox", "Checked files",
                         multiple = FALSE,
                         selectize = TRUE,
                         choices = fileHandler$filenames[fileHandler$fileChecked])
    })

    # Gets the available files in a directory and colors them based on whether
    # they have been processed or not
    available_files <- shiny::reactive({
      infiles = list.files(input$inputDirInput)
      hasOutput = infiles %in% list.files(input$outputDirInput)
      colors = ifelse(hasOutput, ifelse(input$dark_mode == "dark", 'white', "black"), "red")
      names(colors) <- infiles
      colors <- jsonlite::toJSON(as.list(colors))
      colors
    })

    # Renders the selectizeInput for all available files.
    output$availableFilesUI <- shiny::renderUI({
      shiny::selectizeInput("fileSelectBox", "Files Available (red = not processed yet)",
                            multiple = FALSE,
                            choices = list.files(input$inputDirInput),
                            options = list(
                              render =
                                I(sprintf("{
        item: function(item, escape) {
          var colors = %s;
          var color = colors[item.label];
          return '<div style=\"color' + color + '\">' + item.label + '</div>';
        },
        option: function(item, escape) {
          var colors = %s;
          var color = colors[item.label];
          return '<div style=\"color:' + color + '\">' + item.label + '</div>';
        }
      }", available_files(), available_files()))
                            )
      )
    })

    # Adds a slider for the plot point size
    output$sizeSliderUI <- shiny::renderUI({
      # if (is.null(fileHandler$filename))
      #   return(NULL)
      shiny::sliderInput("sizeSlider", "Point Size", min = 1, max = 10, value = 3)
    })

    # Adds a slider for the plot point transparency. The step is set to 1 divided
    # by the number of files in the dataset.
    output$alphaSliderUI <- shiny::renderUI({
      # if (is.null(fileHandler$filename))
      #   return(NULL)
      shiny::sliderInput("alphaSlider", "Transparency", min = 0, max = 1, value = 1,
                         step = round(1/length(fileHandler$filenames),3),ticks = FALSE)
    })

    # The unedited and edited selectInput boxes' default behavior will change
    # the plotted file to whatever is selected (=is displayed in the box). This
    # behavior needs to be suppressed since the choices dynamically change as
    # the user checks files. This is done by using shinyjs::onevent to only change
    # the plotted file when the user clicks on an option.
    shinyjs::onevent(id = "uneditedFileSelectUI",event = "change",
                     expr= {
                       message("Click")
                       if (!is.null(input$uneditedFileSelectBox) && !identical(input$uneditedFileSelectBox, character(0))) {
                         if (sum(fileHandler$isPlotted) == 1)
                           fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
                         fileHandler$isPlotted[] <- FALSE
                         fileHandler$isPlotted[fileHandler$filenames == input$uneditedFileSelectBox] <- TRUE
                         # updatePlotSettingsData()
                       }
                     })

    shinyjs::onevent(id = "editedFileSelectUI",event = "change",
                     expr= {
                       message("Click")
                       if (!is.null(input$editedFileSelectBox) && !identical(input$editedFileSelectBox, character(0))) {
                         if (sum(fileHandler$isPlotted) == 1)
                           fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
                         fileHandler$isPlotted[] <- FALSE
                         fileHandler$isPlotted[fileHandler$filenames == input$editedFileSelectBox] <- TRUE
                         # updatePlotSettingsData()
                       }
                     })



    # When the user clicks the Check off Files button, all files currently displayed
    # will have their fileChecked values set to TRUE.
    shiny::observeEvent(input$checkVisibleFilesButton, {
      message("Check visible files pressed")
      if (!is.null(fileHandler$filenames) && any(fileHandler$isPlotted)) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }
    })

    # Reactive value to change the icon for the Flag Samples button,
    # this is used to change the icon when the button is pressed.
    flagSamplesIcon <- shiny::reactiveValues(value = "flag")

    # Renders the Flag Samples button with the correct icon
    output$flagSamplesButtonLabel <- shiny::renderUI({
      tags$span(shiny::icon(flagSamplesIcon$value), "Flag Samples")
    })

    output$saveButtonLabel <- shiny::renderUI({
      tags$span(shiny::icon(saveIcon$value), "Save File")
    })

    # When the user clicks the Flag Samples button, all files in the loaded
    # dataset will be checked for potential errors. These are added to the
    # flagged_samples column. So the user can can tell that the process worked,
    # the button will change color and the icon will change to a checkmark.
    shiny::observeEvent(input$flagSamplesButton, {
      message("Flag Samples Pressed")
      if (!is.null(loadedFile$data)) {
        loadedFile$data <- flag_potential_errors(loadedFile$data,
                                                 .unique_file = input$filenameColumnInput,
                                                 .hz = input$yValColumnInput,
                                                 .time = input$xValColumnInput,
                                                 .samplerate = 10)
        if (!data.table::is.data.table(loadedFile$data))
          loadedFile$data <- data.table(loadedFile$data)
        # updatePlotSettingsData()
        shinyjs::runjs('document.getElementById("flagSamplesButton").style.backgroundColor = "#18864b";')
        shinyjs::runjs('document.getElementById("flagSamplesButton").style.color = "white";')
        flagSamplesIcon$value <- "check"
      }
    })

    # When the user clicks the Send to Praat button, all files currently displayed
    # in the editor will be sent to Praat
    shiny::observeEvent(input$sendToPraatButton, {
      message("Send to Praat Pressed")
      if (!is.null(fileHandler$isPlotted) & !is.null(input$audioDirInput) & !is.null(input$fileNameGlue)) {

        # Use the columns of the loaded data and the provided glue string to
        # send the files currently displayed in the editor to Praat
        files_to_open <- glue::glue(input$fileNameGlue,
                                    .envir = loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],])
        files_to_open <- file.path(input$audioDirInput, unique(files_to_open))

        if (!is.null(input$textgridDirInput))
          files_to_open <- c(files_to_open, gsub(".wav$", ".TextGrid", files_to_open))

        systemcall <- paste(input$pathToPraat,
                            "--new-open",
                            paste0(files_to_open[file.exists(files_to_open)], collapse = " "),
                            sep = " ")
        message(systemcall)
        base::system(systemcall, wait = FALSE)
      }
    })


    output$cwd <- shiny::renderText({
      getwd()
    })

    shinyjs::onevent(id = "plotPanelRatio",event = "mouseleave",
                     expr= {
                       # message("Changing size of plot window")
                       vals <- paste0(c(input$plotPanelRatio, 1-input$plotPanelRatio), "fr", collapse = " ")
                       message(vals)
                       shinyjs::runjs(paste0('document.getElementById("plotPanel").style.gridTemplateRows = \"', vals , '";'))
                     })

  }



  shiny::shinyApp(ui, server)
}

