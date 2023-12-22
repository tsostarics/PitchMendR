#' Run pulse editor GUI
#'
#' Runs the shiny app
#'
#' @param ... not used
#'
#' @return Nothing
#' @export
#'
#' @importFrom shiny tags isolate
openEditor <- function(...) {
  ui <- bslib::page_navbar(

    id = "navbar",
    title = "Pitch Editor",
    selected = "Setup",
    collapsible = TRUE,
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      shinyjs::useShinyjs(), # Placed here to avoid a warning if placed above a tab
      keys::useKeys(),
      # The keys here should match the default keybindings set up in the server
      keys::keysInput("keys",keys = c("f",
                                      "r",
                                      "e",
                                      "s",
                                      "q",
                                      "w",
                                      "b",
                                      "d",
                                      "a",
                                      "ctrl+z",
                                      "command+z",
                                      "space")
      ),
      title = "Tools",
      shiny::actionButton(
        inputId = "loadFileButton",
        label = "Load File",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
        shiny::icon("upload")
      ),
      shiny::actionButton(
        inputId = "sendToPraatButton",
        label = "Open Files in Praat",
      ),
      shinyWidgets::materialSwitch(inputId = "hideToggleInput",label="Hide transform", value = FALSE,status = "info"),
      shiny::actionButton(
        inputId = "undoTransformButton",
        label = "Undo Transform"
      ),
      shiny::actionButton(
        inputId = "checkVisibleFilesButton",
        icon = shiny::icon('check'),
        label = "off plotted files"
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
      .manual-code code{
      color: #c10000;
      background-color:#f8f8f8;
      }
    "))
      ),
    title = "Editor",
    shiny::fluidRow(
      bslib::layout_column_wrap(
        width= NULL,
        height = NULL, #fill = TRUE,
        heights_equal = "row",
        style = htmltools::css(grid_template_columns = "275px 9fr"),
        bslib::card( height = "88vh",
                     title = "Plot Settings",
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
                     shiny::textOutput(outputId = "nFilesPlotted"),
                     shiny::actionButton(
                       inputId = "goToBrushButton",
                       label = "Plot Brushed Files"
                     ),
                     shiny::verbatimTextOutput(outputId = "brushedFileNames"),
                     # width = '10vw'
        ),
        bslib::card(shinyjqui::jqui_resizable(shiny::plotOutput(outputId = "pulsePlot", click = "plot_click", brush = "plot_brush", height = "70%")),
                    fill = TRUE,
                    height="88vh",
                    width = '80vw',
                    # shiny::fluidRow(#style = "height:20vh;width:90vw",
                    # Adjust the gap space between the buttons
                    tags$head(tags$style(shiny::HTML(".bslib-gap-spacing { gap: 8px; } "))),
                    bslib::layout_columns(height = "20%",width = '80vw',fillable = TRUE,id = "controlButtons",
                                          bslib::card(fill = TRUE,
                                                      shiny::fluidRow(
                                                        shiny::actionButton(width = "98%", inputId = "showLineButton", label = "Show Line", style = "margin-left:1%;margin-right:1%")),
                                                      shiny::fluidRow(
                                                        shiny::actionButton(width = "28%", inputId = "prevButton", label = "<", style = "margin:1%;margin-top:0%;margin-bottom:0"),
                                                        shiny::actionButton(width = "38%", inputId = "saveButton", label = shiny::uiOutput(outputId = "saveButtonLabel"), style = "margin:1%;margin-top:0%;margin-bottom:0"),
                                                        shiny::actionButton(width = "28%", inputId = "nextButton", label = ">", style = "margin:1%;margin-top:0%;margin-bottom:0")
                                                      )

                                          ),
                                          bslib::card(fill = TRUE,
                                                      shiny::fluidRow(
                                                        shiny::actionButton(width = "98%", inputId = "toggleButton", label = "Toggle Pulses", style = "margin-left:1%;margin-right:1%")),
                                                      shiny::fluidRow(
                                                        shiny::actionButton(width = "48%", inputId = "keepButton", label = "Keep", style = "margin:1%;margin-top:0%;margin-bottom:0"),
                                                        shiny::actionButton(width = "48%", inputId = "removeButton", label = "Remove", style = "margin: 1%;margin-top:0%;margin-bottom:0")),
                                                      shiny::fluidRow(
                                                        shiny::actionButton(width = "48%", inputId = "halfButton", label = "Halve Pulses", style = "margin: 1%;margin-top:0%;margin-bottom:0"),
                                                        shiny::actionButton(width = "48%",inputId = "doubleButton", label = "Double Pulses", style = "margin: 1%;margin-top:0%;margin-bottom:0"))
                                          )
                    )
                    # )
        )
      )
    )
    ),
    bslib::nav_panel(
      title = "Settings",
      shiny::fluidRow(height = "100%",
                      shiny::column(width = 3,
                                    bslib::card(
                                      height = '88vh',fill = TRUE,
                                      title = "Press button to set column",
                                      shiny::selectizeInput("filenameColumnInput",
                                                            label ="Column name containing individual files",
                                                            choices = "Filename",
                                                            selected = "Filename",
                                                            multiple = FALSE,
                                                            width = "100%"),
                                      shiny::selectizeInput("xValColumnInput",
                                                            label ="X-value column name",
                                                            choices = "t_ms",
                                                            selected = "t_ms",
                                                            multiple = FALSE,
                                                            width = "100%",),
                                      shiny::selectizeInput("yValColumnInput",
                                                            label ="Y-value column name",
                                                            choices = "f0",
                                                            selected = "f0",
                                                            multiple = FALSE,
                                                            width = "100%",),
                                      submitTextInput("selectionColumnInput",
                                                      label = "Column name for keep/remove annotations (will be added if it doesn't exist)",
                                                      value = "keep_pulse",
                                                      width = "100%"
                                      ),
                                      shinyWidgets::materialSwitch(
                                        inputId = "saveOptionButton",
                                        label = "Save on file navigation:",
                                        value = TRUE,
                                        inline = TRUE,
                                        status = "info"
                                      ),
                                      shinyWidgets::materialSwitch(
                                        inputId = "useKeysToggle",
                                        label = "Use keyboard shortcuts:",
                                        value = TRUE,
                                        inline = TRUE,
                                        status = "info"
                                      )
                                    )
                      ),
                      shiny::column(width = 3,
                                    bslib::card(
                                      height = '88vh',
                                      fill = TRUE,
                                      title = "Color Settings",
                                      "Override default color settings below:",
                                      colorUI("colors"),
                                      shinyWidgets::materialSwitch(
                                        inputId = "useFlaggedColumnToggle",
                                        label="Color points by column:",
                                        value = FALSE,
                                        inline = TRUE,
                                        status = "info"),
                                      # Tabset panel to hide/show color code column input
                                      # based on whether the useFlaggedColumnToggle is
                                      # set to TRUE. Needs to be done this way instead of
                                      # using uiOutput, otherwise the plot won't
                                      # render until the user manually goes to the
                                      # settings tab so the selectize input
                                      # initializes
                                      tabsetPanel(type = "hidden",
                                                  id = "switchColorCode",
                                                  tabPanelBody("showColorCodeColumnInput",
                                                               shiny::selectizeInput(
                                                                 inputId = "colorCodeColumnInput",
                                                                 label = NULL,
                                                                 choices = "flagged_samples",
                                                                 selected = "flagged_samples",
                                                                 multiple = FALSE,
                                                                 width = "100%"
                                                               )),
                                                  tabPanelBody("hideColorCodeColumnInput", NULL)))
                      ),
                      shiny::column(width = 6,
                                    bslib::card(
                                      height = '88vh',fill = TRUE,
                                      title = "Instructions",
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
                      )
      )),
    bslib::nav_panel(
      title = "Progress",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(""),
        bslib::card_body(
          shiny::actionButton(
            inputId = "refreshProgressButton",
            label = "Refresh Diagnostics"
          ),
          shiny::actionButton(
            inputId = "plotUneditedFilesButton",
            label = "Send Unedited Files to Editor"
          ),
          "Percentage of pulses removed from current file:",
          shiny::textOutput(outputId = "percentRemovedText"),
          "Change in average variance of semitone differences (+/- 2 se)",
          shiny::verbatimTextOutput(outputId = "changeInVarianceOutput"),
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

          shiny::fluidRow(
            shiny::column(width = 6,
                          bslib::card(
                            title = "Directory Settings",
                            "Current working directory:",
                            shiny::verbatimTextOutput(outputId = "cwd"),
                            shiny::textInput(
                              width = "100%",
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
                            # span(
                            #   `data-toggle` = "tooltip", `data-placement` = "right",
                            #   title = "A tooltip",
                            #   icon("info-circle")
                            # )
                            span(style = "display:inline-block;",
                                 "Glue string to match file names to directory",
                                 id = "glueQuestion",
                                 span(style = "cursor:pointer;", shiny::icon("circle-question"))),
                            shiny::textInput(
                              inputId = "fileNameGlue",
                              label = NULL,
                              value = "{Speaker}_{Filename}.wav"
                            ),
                            "The following are only used when opening files in Praat.",
                            shiny::textInput(
                              inputId = "audioDirInput",
                              label  = "Audio Directory",
                              value = "./audio"
                            ),
                            shiny::textInput(
                              inputId = "textgridDirInput",
                              label = "TextGrid Directory",
                              value = "./audio"
                            )
                          )
            ),
            shiny::column(width = 6,
                          bslib::card(
                            title = "Flagging Samples",
                            shiny::markdown(
                              mds = c(
                                "## Flagging samples",
                                "",
                                "After loading your data, you can use an automated method to flag potential tracking errors.
                    This method is based on identifying octave jumps between adjacent samples.
                    Note that this is not perfect and may flag samples that are not tracking errors and miss samples that are tracking errors.
                    It is best used to identify regions of interest that should be investigated for errors.",
                    "",
                    "If the column `F0_semitones` already exists, it will be used to identify errors.
                    If not, this column will be added by computing semitones from the mean pitch of all of the speaker's files.
                    Check the settings tab for the column names used for the time, pitch, and filename values.",
                    "",
                    "The algorithm assumes that time is provided in millisecond units (e.g., t=1410ms).
                    If time is provided in seconds (e.g., t=1.41s) then the time column will be converted to milliseconds.
                    The sampling rate is computed automatically, but if your time values are not in either seconds or milliseconds,
                    then it may not work correctly.",
                    "",
                    "The column `flagged_samples` will be added if it doesn't exist.
                    Once the column is added, or if it already exists, the button will turn green.
                    Clicking it again will rerun the algorithm, and previous values will be overwritten."
                              )
                            ),
                    shiny::actionButton(
                      inputId = "flagSamplesButton",
                      shiny::uiOutput(outputId = "flagSamplesButtonLabel")
                    )
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
                                          themeColors = NULL,
                                          setColors = NULL)
    fileHandler <- shiny::reactiveValues(filenames = NULL,
                                         isPlotted = NULL,
                                         fileChecked = NULL)
    filesBrushed <- shiny::reactiveValues(filenames = NULL)
    uneditedFiles <- shiny::reactiveValues(filenames = NULL)
    lastTransformation <- shiny::reactiveValues(pulse_ids = NULL)
    loadedFile <- shiny::reactiveValues(data = NULL)
    transformedColumn <- shiny::reactiveValues(name = NULL)
    plotFlag <- shiny::reactiveValues(value = TRUE)


    # Default keybindings, see https://craig.is/killing/mice for valid keys
    boundKeys <- shiny::reactiveValues(
      keys = list("f" = keyBindAction(togglePulses, "[F] Pressed (Toggle)"),
                  "r" = keyBindAction(removePulses, "[R] Pressed (Remove)"),
                  "e" = keyBindAction(keepPulses, "[E] Pressed (Keep)"),
                  "s" = keyBindAction(toggleShowLine, "[S] Pressed (Show Line)"),
                  "q" = keyBindAction(goToPreviousFile, "[Q] Pressed (Previous File)"),
                  "w" = keyBindAction(goToNextFile, "[W] Pressed (Next File)"),
                  "b" = keyBindAction(plotBrushed, "[B] Pressed (Plot Brushed)"),
                  "d" = keyBindAction(doublePulses, "[D] Pressed (Double Pulses)"),
                  "a" = keyBindAction(halvePulses, "[A] Pressed (Halve Pulses)"),
                  "ctrl+z" = keyBindAction(undoTransformation, "[Ctrl+Z] Pressed (Undo Transform)"),
                  "command+z" = keyBindAction(undoTransformation, "[Command+Z] Pressed (Undo Transform)"),
                  "space" = keyBindAction(plotMatches, "[Space] pressed (Plot Matches)"))
    )



    observeEvent(input$keys, {
      # Key bindings only apply on the editor page
      if (input$navbar != "Editor" | (!is.null(input$useKeysToggle) && !input$useKeysToggle))
        return(NULL)
      boundKeys$keys[[input$keys]]()
    })


    colorServer("colors",
                plotSettings,
                updatePlot,
                reactive(input$dark_mode),
                reactive(input$loadFileButton))

    getBrushedPoints <- shiny::reactive({
      yval <- transformedColumn$name
      if (!is.null(input$hideToggleInput) && input$hideToggleInput)
        yval <- input$yValColumnInput

      shiny::brushedPoints(loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                           input$plot_brush,
                           xvar = input$xValColumnInput,
                           yvar = yval)
    })

    # The plot render will take on these as dependencies, too
    no_missing_plot_inputs <- reactive({
      !any(c(is.null(plotFlag$value),
             is.null(loadedFile$data),
             is.null(input$alphaSlider),
             is.null(input$sizeSlider),
             is.null(input$yValColumnInput),
             is.null(input$xValColumnInput),
             is.null(input$filenameColumnInput),
             is.null(input$selectionColumnInput),
             is.null(input$useFlaggedColumnToggle),
             is.null(input$colorCodeColumnInput)))
    })

    output$pulsePlot <- shiny::renderPlot({
      message('Rerendering')
      if (no_missing_plot_inputs()) {

        plot_subset <- loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],]
        # These will update when the user changes the color manually with the
        # color pickers or if the theme is changed.
        color_values <- plotSettings$setColors[2:3]
        lineColor <- plotSettings$setColors[1]

        # If the user wants to use the flagged column, make sure it exists
        if (input$useFlaggedColumnToggle && input$colorCodeColumnInput %in% colnames(plot_subset)) {
          fx <- \(x) x
          if (length(unique(plot_subset[[input$colorCodeColumnInput]])) <= 2) {
            fx <- base::factor
          }

          plot_subset[, (input$colorCodeColumnInput) := fx(get(input$colorCodeColumnInput))]
        }

        yval <- transformedColumn$name
        if (!is.null(input$hideToggleInput) && input$hideToggleInput)
          yval <- input$yValColumnInput

        # Set up the main aesthetics for the plot
        p <-
          ggplot2::ggplot(plot_subset,
                          ggplot2::aes(x = !!rlang::sym(input$xValColumnInput),
                                       y = !!rlang::sym(yval),
                                       group = !!rlang::sym(input$filenameColumnInput)))

        # Add the line if the user wants it, this should go under the points
        if (plotSettings$showLine) {
          p <- p +
            ggplot2::geom_line(data =plot_subset[plot_subset[[input$selectionColumnInput]],], color = lineColor)
        }

        # Add the points, if the user wants to use the flagged column, use it to color the points
        # otherwise just use the keep_pulse column to redundantly code that information
        if (input$useFlaggedColumnToggle && input$colorCodeColumnInput %in% colnames(plot_subset)) {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(input$colorCodeColumnInput), shape = !!rlang::sym(input$selectionColumnInput)),
                                size = input$sizeSlider,
                                alpha = input$alphaSlider)
        } else {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(input$selectionColumnInput), shape = !!rlang::sym(input$selectionColumnInput)),
                                size = input$sizeSlider,
                                alpha = input$alphaSlider)
        }

        # If the color column is binary, use the colors specified by the user,
        # otherwise just use the default colors
        if (!input$useFlaggedColumnToggle ||
            length(unique(plot_subset[[input$colorCodeColumnInput]])) <= 2) {
          if (input$useFlaggedColumnToggle)
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

    togglePulses <- reactive({
      selectedPoints$data <- getBrushedPoints()

      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[[input$selectionColumnInput]][vals_to_change] <- !loadedFile$data[[input$selectionColumnInput]][vals_to_change]
        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # When the user clicks the toggle button, toggle the logical keep_pulse value
    shiny::observeEvent(input$toggleButton, {
      message("Toggle Pressed")
      togglePulses()
    })

    shiny::observeEvent(input$colorCodeColumnInput, {
      if (is.null(loadedFile$data) | is.null(input$colorCodeColumnInput) | is.null(input$useFlaggedColumnToggle))
        return(NULL)

      if (input$useFlaggedColumnToggle)
        updatePlot()
    })

    shiny::observeEvent(input$useFlaggedColumnToggle, {
      if (input$useFlaggedColumnToggle){
        updateTabsetPanel(inputId = "switchColorCode", selected = "showColorCodeColumnInput")
      } else {
        updateTabsetPanel(inputId = "switchColorCode", selected = "hideColorCodeColumnInput")
      }

      updatePlot()
    })


    # Toggle point on click
    shiny::observeEvent(input$plot_click, {
      if (is.null(loadedFile$data))
        return(NULL)
      clickedPoint <- shiny::nearPoints(loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                                        input$plot_click,
                                        xvar = input$xValColumnInput,
                                        yvar = transformedColumn$name,
                                        addDist = TRUE)
      clickedPoint$pulse_id

      if (!is.null(clickedPoint) & length(clickedPoint$pulse_id) != 0) {
        first_id <- clickedPoint$pulse_id[which.min(clickedPoint$dist_)] # Get the pulse_id of the closest point
        loadedFile$data[[input$selectionColumnInput]][first_id] <- !loadedFile$data[[input$selectionColumnInput]][first_id]
        clickedPoint <- NULL
        updatePlot()
      }
    })

    keepPulses <- reactive({
      selectedPoints$data <- getBrushedPoints()

      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, (input$selectionColumnInput) := TRUE]
        # updatePlotSettingsData()
        selectedPoints$data <- NULL
        updatePlot()

      }})

    removePulses <- reactive({
      selectedPoints$data <- getBrushedPoints()

      # Toggle the color of the selected points
      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, (input$selectionColumnInput) := FALSE]
        # updatePlotSettingsData()

        selectedPoints$data <- NULL
        updatePlot()

      }
    })

    # When the user clicks the keep button, set the logical keep_pulse value to TRUE
    shiny::observeEvent(input$keepButton, {
      message("Keep Pressed")
      keepPulses()

    })

    # When the user clicks the remove button, set the logical keep_pulse value to FALSE
    shiny::observeEvent(input$removeButton, {
      message("Remove Pressed")
      removePulses()
    })

    # Whenever the user toggles the dark mode, update the colors of the load file
    # button. It should be blue no matter what if no file is loaded, then fade
    # to an unselected gray color after a file is loaded.


    # When the user clicks the load file button, load the file and initialize
    # different columns and reactive values
    # Install and load the data.table package

    set_selectize_choices <- function(session,
                                      inputId,
                                      data_ref,
                                      input_value,
                                      add = FALSE,
                                      add_with = TRUE) {
      reactive({
        if (is.null(data_ref$data))
          return(NULL)

        if (add) {
          if (!input_value %in% colnames(data_ref$data))
            data_ref$data[, (input_value) := add_with]
        }

        updateSelectizeInput(session,
                             inputId = inputId,
                             choices = colnames(data_ref$data),
                             selected = ifelse(input_value %in% colnames(data_ref$data),
                                               input_value,
                                               colnames(data_ref$data)[1]))
      }
      )
    }

    shiny::observeEvent(input$yValColumnInput, {
      if (is.null(loadedFile$data))
        return(NULL)
      changeTransformedColumn()
    })

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
      loadedFile$data <- NULL # If previous data was loaded, throw it out
      loadedFile$data <- data.table::fread(file_to_load)  # Use fread from data.table package

      set_selectize_choices(session, "filenameColumnInput", loadedFile, input$filenameColumnInput)()
      set_selectize_choices(session, "xValColumnInput", loadedFile, input$xValColumnInput)()
      set_selectize_choices(session, "yValColumnInput", loadedFile, input$yValColumnInput)()


      # If the file doesn't contain the specified columns, return null and move
      # to the settings page
      if (!all(c(input$filenameColumnInput, input$xValColumnInput, input$yValColumnInput) %in% colnames(loadedFile$data))) {
        message("File doesn't contain the specified columns")
        loadedFile$data <- NULL
        shiny::updateNavbarPage(session, "navbar", "Settings")
        return(NULL)
      }

      # updateLoadFileColors()

      data.table::setorderv(loadedFile$data, cols = c(input$filenameColumnInput, input$xValColumnInput))  # Use setorder from data.table package

      if (!file.exists(outFile))
        loadedFile$data[, (input$selectionColumnInput) := get(input$yValColumnInput) > 2]  # Use := operator from data.table package

      if (!"pulse_id" %in% colnames(loadedFile$data))
        loadedFile$data[, pulse_id := .I]  # Use .I from data.table package

      if (!"pulse_transform" %in% colnames(loadedFile$data)){
        loadedFile$data[, pulse_transform := 1.0]
      }

      changeTransformedColumn()
      # transformedColumn$name <- paste(input$yValColumnInput, "transformed", sep = "_")
      # loadedFile$data[, (transformedColumn$name) := pulse_transform * get(input$yValColumnInput)]  # Use get from data.table package

      fileHandler$filenames <- unique(loadedFile$data[[input$filenameColumnInput]])
      fileHandler$isPlotted <- rep(TRUE, length(fileHandler$filenames))
      # fileHandler$isPlotted[1] <- TRUE

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

      # If we've loaded another file in the same session then we need
      # to reset flag samples button

      if ("flagged_samples" %in% colnames(loadedFile$data)) {
        shinyjs::addClass(id = "flagSamplesButton", class = "btn-success")
        flagSamplesIcon$value <- "check"
        shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
      } else {
        shinyjs::removeClass(id = "flagSamplesButton", class = "btn-success")
        flagSamplesIcon$value <- "flag"
      }

      set_selectize_choices(session, "colorCodeColumnInput", loadedFile, input$colorCodeColumnInput)()
      updatePlot()

      # shiny::updateNavbarPage(session, "navbar", "Editor")
    })

    toggleShowLine <- reactive({
      plotSettings$showLine <- !plotSettings$showLine
    })

    # Toggle whether the line should be shown or not
    shiny::observeEvent(input$showLineButton, {
      toggleShowLine()
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


    goToPreviousFile <- reactive({
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

      if (input$saveOptionButton) {
        saveData(file.path(input$outputDirInput, input$fileSelectBox))
      }
    })

    goToNextFile <- reactive({
      # Get the maximum index of the files that are currently plotted,
      # if we're already at the last file, wrap around to the first file
      current_max <- max(which(fileHandler$isPlotted))
      if (current_max >= length(fileHandler$filenames))
        current_max <- 0

      # Check off the file that's currently plotted before we move to the next file
      if (sum(fileHandler$isPlotted) == 1) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }

      fileHandler$isPlotted[] <- FALSE
      fileHandler$isPlotted[current_max + 1] <- TRUE

      # If we have the save-on-next option enabled, save the data
      if (input$saveOptionButton) {
        saveData(file.path(input$outputDirInput, input$fileSelectBox))
      }
    })

    # When the user clicks the next button, plot the next file alphabetically
    shiny::observeEvent(input$nextButton, {
      goToNextFile()

    })

    # When the user clicks the previous button, plot the previous file alphabetically
    shiny::observeEvent(input$prevButton, {
      goToPreviousFile()

    })



    # Display the number of files/contours that are currently plotted
    output$nFilesPlotted <- shiny::renderText({
      paste0("# of files shown: ", sum(fileHandler$isPlotted))
    })

    # When the user clicks the plot matches button, plot the files that match the
    # regex in the filter regex box
    shiny::observeEvent(input$plotMatchesButton, {
      message("Plot Matches Pressed")
      plotMatches()
    })

    plotMatches <- reactive({
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

    plotBrushed <- reactive({
      brushed_regex <- paste0("(", filesBrushed$filenames, ")", collapse = "|")
      fileHandler$isPlotted <- grepl(brushed_regex,fileHandler$filenames)

      message(paste0("Plotting ", sum(fileHandler$isPlotted), " files from selection"))
    })

    # When the user clicks the go to brushed button, plot only the files that the
    # user selected in the editor
    shiny::observeEvent(input$goToBrushButton, {
      message("Go to brushed pressed")
      plotBrushed()
    })

    # Refresh the diagnostics pane when the user clicks the refresh button
    shiny::observeEvent(input$refreshProgressButton,{
      filtered_data <- loadedFile$data[get(input$yValColumnInput) > 2]

      output$percentRemovedText <- shiny::renderText({
        paste0(round(sum(!filtered_data[[input$selectionColumnInput]]) / nrow(filtered_data)*100, 2), "%")
      })

      output$nEditedFilesText <- shiny::renderText({
        n_edited <- loadedFile$data[, .(n_edited = sum(!get(input$selectionColumnInput))), by = c(input$filenameColumnInput)][n_edited > 0, .N]
        paste0(n_edited, " / ", length(unique(filtered_data[[input$filenameColumnInput]])))
      })

      output$changeInVarianceOutput <- shiny::renderText({
        variance_values <-
          loadedFile$data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(input$filenameColumnInput))) |>
          dplyr::reframe(original_variance = .var_of_diffs(.data[[input$yValColumnInput]][.data[[input$yValColumnInput]]>2]),
                         new_variance = .var_of_diffs(.data[[transformedColumn$name]][.data[[input$selectionColumnInput]]])) |>
          dplyr::ungroup()

        mean_se1 <- ggplot2::mean_se(variance_values$original_variance, mult = 2)
        mean_se2 <- ggplot2::mean_se(variance_values$new_variance, mult = 2)

        paste0("Original Variance: ", round(mean_se1['y'], 2), "[", round(mean_se1['ymin'], 2), ",", round(mean_se1['ymax']), "]\n",
               "New Variance: ", round(mean_se2['y'], 2), "[", round(mean_se2['ymin'], 2), ",", round(mean_se2['ymax']), "]\n")

      })

      output$uneditedFilesText <- shiny::renderText({
        uneditedFiles$filenames <-
          filtered_data[, .(n_edited = sum(!get(input$selectionColumnInput))), by = c(input$filenameColumnInput)][n_edited == 0, unique(.SD[[input$filenameColumnInput]])]

        paste(uneditedFiles$filenames, collapse = "\n")
      })

    })

    updatePlot <- shiny::reactive({
      observe("keepFalseColor")
      plotFlag$value <- !plotFlag$value
    })

    doublePulses <- shiny::reactive({
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id


      loadedFile$data[vals_to_change, pulse_transform := pulse_transform * 2.0]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change
      updatePlot()
    })

    halvePulses <- shiny::reactive({
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id

      # as.numeric needed to avoid implicit coercion to integer,
      # which turns the transformation from 0.5 to 0
      loadedFile$data[, pulse_transform:= as.numeric(pulse_transform)][vals_to_change, pulse_transform := pulse_transform * 0.5]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]


      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change
      updatePlot()
    })

    # Multiply selected points by 2 (fixes halving errors)
    shiny::observeEvent(input$doubleButton, {
      message("Double Pressed")
      doublePulses()
    })

    changeTransformedColumn <- shiny::reactive({
      transformedColumn$name <- paste(input$yValColumnInput, "transformed", sep = "_")
      loadedFile$data[, (transformedColumn$name) := pulse_transform * get(input$yValColumnInput)]
    })

    shiny::observeEvent(input$yValColumnInputButton, {
      if (is.null(loadedFile$data))
        return(NULL)
      message(input$yValColumnInput)
      changeTransformedColumn()
      updatePlot()

    })

    # Multiply selected points by 0.5 (fixes doubling errors)
    shiny::observeEvent(input$halfButton, {
      message("Halve Pressed")

    })

    undoTransformation <- reactive({
      if (!is.null(lastTransformation$pulse_ids)) {
        loadedFile$data[lastTransformation$pulse_ids, pulse_transform := 1.0]
        loadedFile$data[lastTransformation$pulse_ids, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

        lastTransformation$pulse_ids <- NULL
        selectedPoints$data <- NULL
        updatePlot()
      }
    })

    # Undo the last transformation, resetting the transformation to 1
    shiny::observeEvent(input$undoTransformButton, {
      message("Undo Pressed")
      undoTransformation()
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

    shinyjs::onclick(id = "glueQuestion", {
      shinyWidgets::show_alert(
        title = "Glue strings",
        type = 'info',
        html = TRUE,
        text = tags$div(class = "manual-code",
                        shiny::markdown(
                          mds = c(
                            "The [glue](https://glue.tidyverse.org/) package provides a convenient way to interpolate strings, similar to Python's f-strings.",
                            "Expressions in {curly braces} will be evaluated and the result will be inserted into the string.",
                            "Here, you can only work with the column names in your data.",
                            "",
                            "Let's say your speaker IDs and file names are in columns named
          `Speaker` and `Filename` respectively, with example values `spkr01` and `11_rise.wav`.
          The string `audio/Speaker_{Speaker}_{Filename}` will then be evaluated as
          `audio/Speaker_spkr01_11_rise.wav`"
                          ))))

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
        flagSamplesIcon$value <- "hourglass"
        loadedFile$data <-
          flag_potential_errors(loadedFile$data,
                                .unique_file = input$filenameColumnInput,
                                .hz = input$yValColumnInput,
                                .time = input$xValColumnInput,
                                .samplerate = NA)
        loadedFile$data[['flagged_samples']] <-factor(loadedFile$data[['flagged_samples']], levels = c(0,1))
        if (!data.table::is.data.table(loadedFile$data))
          loadedFile$data <- data.table(loadedFile$data)

        shinyjs::addClass(id = 'flagSamplesButton',class = "btn-success")
        flagSamplesIcon$value <- "check"
        shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
      }
    })

    # When the user clicks the Send to Praat button, all files currently displayed
    # in the editor will be sent to Praat
    shiny::observeEvent(input$sendToPraatButton, {
      message("Send to Praat Pressed")
      if (!is.null(fileHandler$isPlotted) & !is.null(input$audioDirInput) & !is.null(input$fileNameGlue)) {

        # Use the columns of the loaded data and the provided glue string to
        # send the files currently displayed in the editor to Praat
        files_to_open <- glue::glue_data_safe(loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],],
                                              input$fileNameGlue)
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

  }

  shiny::shinyApp(ui, server)
}

