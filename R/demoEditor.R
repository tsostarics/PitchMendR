#' Open web-version editor
#'
#' Runs the web app version of the app
#'
#' @param ... not used
#' @param input_directory Directory containing the csv files to load
#' @param output_directory Directory to save annotated csv files to
#' @param audio_directory Directory containing .wav files to play or send to
#' praat
#' @param textgrid_directory Directory containing .TextGrid files to send to
#' praat
#' @param praat_path For Windows, the path to a Praat executable. For linux and
#' mac, this should just be Praat assuming it's available as a terminal command.
#'
#' @return Nothing
#' @export
#'
#' @importFrom shiny tags isolate reactive moduleServer observeEvent icon req reactiveValues observe
#' @importFrom rlang sym
#' @importFrom stats median
demoEditor <- function(
    input_directory = get_example_f0_data("."),
    output_directory = "./",
    audio_directory = "./audio",
    textgrid_directory = "./audio",
    praat_path = "./Praat.exe",
    ...) {
  options(shiny.maxRequestSize = 10 * 1024^2)
  # Keyboard shortcuts run in the RStudio viewer will also execute in RStudio,
  # so we'll try to open a new file to avoid cases where keybindings modify
  # an active file.
  # try({
  #   rstudioapi::documentNew("")
  #   messsage("Opening new file to avoid keyboard shortcut conflicts")
  #   })
  ui <- bslib::page_navbar(

    id = "navbar",
    title = "PitchMendR",
    selected = "Setup",
    collapsible = TRUE,
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      tags$head(
        tags$style(
          shiny::HTML("
          .smallPlayButton {
            width: 75%;

          }
          .bigPlayButton {
          width: 100%
          }
          .hideStopButton {
            display: none;
          }
          .showStopButton {
            width: 20%;
            padding-left: 0px;
            padding-right: 0px;
            border: none;
            margin-left: 5.5px;
            padding-top: .7em;
            padding-bottom: .7em;
          }"))
      ),
      shinyjs::useShinyjs(), # Placed here to avoid a warning if placed above a tab
      keys::useKeys(),
      use_keyboardcss(),
      singleton(
        htmltools::includeCSS(
          system.file("cssfiles/button_animation.css", package = "PitchMendR")
        )
      ),
      windowResizeUI("windowListener"),
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
                                      "v",
                                      "p")
      ),
      title = "Tools",

      # praatUI_button("praatIO"),
      tags$span(title = "Toggle to hide doubling/halving transforms",
                shinyWidgets::materialSwitch(inputId = "hideToggleInput",
                                             label= "Hide transform",
                                             value = FALSE,
                                             status = "info")),
      tags$span(title = "Toggle to draw line through removed points",
                shinyWidgets::materialSwitch(inputId = "useRemovedPointsToggleInput",
                                             label= "Show \u25B3s in line",
                                             value = FALSE,
                                             status = "info")),
      shiny::actionButton(
        inputId = "undoTransformButton",
        title = "Click to undo last doubling/halving transform",
        label = "Undo Transform"
      ),
      shiny::actionButton(
        inputId = "checkVisibleFilesButton",
        title = "Click to check off currently plotted files",
        icon = shiny::icon('check'),
        label = "off visible"
      ),
      # playAudioUI("playAudio"),
      tags$span(title = "Select a file that has not been checked yet",
                shiny::uiOutput(outputId = "uneditedFileSelectUI")),
      tags$span(title = "Select a file that has already been checked",
                shiny::uiOutput(outputId = "editedFileSelectUI"))
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
      .form-group {
        margin-bottom: 8px;
      }
    "))
      ),
    title = "Editor",
    shiny::fluidRow(
      bslib::layout_column_wrap(
        width= NULL,
        height = NULL,
        heights_equal = "row",
        style = htmltools::css(grid_template_columns = "275px 9fr"),
        bslib::card( height = "88vh",
                     title = "Plot Settings",
                     shiny::textOutput(outputId = "workingFileOutput"),
                     shiny::uiOutput(outputId = "pitchRangeUI"),
                     tags$span(title = "Drag to change size of points",
                               shiny::sliderInput("sizeSlider", "Point Size", min = 1, max = 10, value = 3)),
                     tags$span(title = "Drag to change transparency of points",
                               shiny::sliderInput("alphaSlider", "Transparency", min = 0, max = 1, value = 1,
                                                  step = 0.05,ticks = FALSE)),
                     tags$span(title = "Enter regular expression to plot matching files",
                               shiny::textInput(
                                 inputId = "filterRegex",
                                 label = "Plot files matching regex",
                                 value = "."
                               )),
                     shiny::actionButton(
                       inputId = "plotMatchesButton",
                       title = "Click to plot files that match regex",
                       label = "Plot Matches"
                     ),
                     shiny::textOutput(outputId = "nFilesPlotted"),
                     shiny::actionButton(
                       inputId = "goToBrushButton",
                       title = "Click to plot selected files",
                       label = "Plot Brushed Files"
                     ),
                     shiny::verbatimTextOutput(outputId = "brushedFileNames"),
                     # width = '10vw'
        ),
        bslib::card(
          shinyjqui::jqui_resizable(shiny::plotOutput(outputId = "pulsePlot",
                                                      click = "plot_click",
                                                      brush = "plot_brush",
                                                      height = "70%"),
                                    options = list(containment = "parent",
                                                   save = TRUE)),
          fill = TRUE,
          height="88vh",
          width = '80vw',
          # shiny::fluidRow(#style = "height:20vh;width:90vw",
          # Adjust the gap space between the buttons
          tags$head(tags$style(shiny::HTML(".bslib-gap-spacing { gap: 8px; } "))),
          bslib::layout_columns(height = "20%",width = '80vw',fillable = TRUE,id = "controlButtons",
                                bslib::card(fill = TRUE,
                                            shiny::fluidRow(
                                              shiny::actionButton(width = "98%",
                                                                  inputId = "showLineButton",
                                                                  label = "Show Line",
                                                                  title = "Click to show/hide contour lines",
                                                                  style = "margin-left:1%;margin-right:1%")),
                                            web_fileNavUI("web_fileNav"),
                                            annotationUI("annotations")
                                ),
                                bslib::card(fill = TRUE,
                                            shiny::fluidRow(
                                              shiny::actionButton(inputId = "toggleButton",
                                                                  label = "Toggle Pulses",
                                                                  title = "Click to toggle currently selected points",
                                                                  style = "margin-left:1%;margin-right:1%",
                                                                  width = "98%")),
                                            shiny::fluidRow(
                                              shiny::actionButton(width = "48%",
                                                                  inputId = "keepButton",
                                                                  label = "Keep",
                                                                  title = "Click to keep selected points",
                                                                  style = "margin:1%;margin-top:0%;margin-bottom:0"),
                                              shiny::actionButton(width = "48%",
                                                                  inputId = "removeButton",
                                                                  label = "Remove",
                                                                  title = "Click to remove selected points",
                                                                  style = "margin: 1%;margin-top:0%;margin-bottom:0")),
                                            shiny::fluidRow(
                                              shiny::actionButton(width = "48%",
                                                                  inputId = "halfButton",
                                                                  label = "Halve Pulses",
                                                                  title = "Click to halve point y-values",
                                                                  style = "margin: 1%;margin-top:0%;margin-bottom:0"),
                                              shiny::actionButton(width = "48%",
                                                                  inputId = "doubleButton",
                                                                  label = "Double Pulses",
                                                                  title = "Click to double point y-values",
                                                                  style = "margin: 1%;margin-top:0%;margin-bottom:0"))
                                )
          )
          # )
        )
      )
    )
    ),
    bslib::nav_panel(
      title = "Settings",
      shiny::fluidRow(class = "h-100",
                      shiny::column(width = 3,
                                    bslib::card(
                                      class = "h-100",
                                      tags$span(title = "Select column that indexes individual files/contours",
                                                shiny::selectizeInput("filenameColumnInput",
                                                                      label ="Column name that identifies individual files",
                                                                      choices = "Filename",
                                                                      selected = "Filename",
                                                                      multiple = FALSE,
                                                                      width = "100%")),
                                      tags$span(title = "Select column that identifies the x-axis for the plot",
                                                shiny::selectizeInput("xValColumnInput",
                                                                      label ="X-value column name",
                                                                      choices = "t_ms",
                                                                      selected = "t_ms",
                                                                      multiple = FALSE,
                                                                      width = "100%")),
                                      tags$span(title = "Select column that identifies the y-axis for the plot",
                                                shiny::selectizeInput("yValColumnInput",
                                                                      label ="Y-value column name",
                                                                      choices = "f0",
                                                                      selected = "f0",
                                                                      multiple = FALSE,
                                                                      width = "100%")),
                                      submitTextInput("selectionColumnInput",
                                                      title = "Click button set column name",
                                                      label = "Column name for keep/remove annotations (will be added if it doesn't exist)",
                                                      value = "keep_pulse",
                                                      width = "100%"),
                                      shiny::markdown(mds = "## UI Options"),
                                      # tags$span(title = "Toggle to save file to disk when plot refreshes with new files",
                                      #           shinyWidgets::awesomeCheckbox(
                                      #             inputId = "saveOptionButton",
                                      #             label = "Save on file navigation (disable to improve responsiveness)",
                                      #             value = FALSE,width = "100%",
                                      #             status = "info"
                                      #           )),
                                      tags$span(title = "Toggle to skip checked files when using previous/next file buttons",
                                                shinyWidgets::awesomeCheckbox(
                                                  inputId = "skipCheckedFilesToggle",
                                                  label = "Skip checked files on file navigation",
                                                  value = TRUE,
                                                  status = "info"
                                                )),
                                      tags$span(title = "Toggle to show annotation buttons in editor pane",
                                                shinyWidgets::awesomeCheckbox(
                                                  inputId = "useBadgesToggle",
                                                  label = "Use tag buttons",
                                                  value = TRUE,
                                                  status = "info"
                                                )),
                                      tags$span(title = "Toggle to show text area in editor pane",
                                                shinyWidgets::awesomeCheckbox(
                                                  inputId = "useNotesToggle",
                                                  label = "Use notepad for annotation",
                                                  value = TRUE,
                                                  status = "info"
                                                )),
                                      tags$span(style = "display:inline-block;",
                                                title = "Toggle to use keyboard shortcuts (click ? for more info)",
                                                HTML(shinyWidgets::awesomeCheckbox(
                                                  inputId = "useKeysToggle",
                                                  label = "Use keyboard shortcuts:",
                                                  value = TRUE,
                                                  status = "info"
                                                ) |> gsub("</label>",
                                                          paste0("</label>\n",
                                                                 span(id = "keysQuestion",
                                                                      style = "cursor:pointer;",
                                                                      shiny::icon("circle-question"))),
                                                          x=_))
                                      ),
                                    )
                      ),
                      shiny::column(width = 3,
                                    bslib::card(
                                      class = "h-100",
                                      title = "Color Settings",
                                      "Override default color settings below:",
                                      colorUI("colors"),
                                      tags$span(title = "Toggle to use column in dataset for color coding",
                                                shinyWidgets::materialSwitch(
                                                  inputId = "useFlaggedColumnToggle",
                                                  label="Color points by column:",
                                                  value = FALSE,
                                                  inline = TRUE,
                                                  status = "info")),
                                      # Tabset panel to hide/show color code
                                      # column input based on whether the
                                      # useFlaggedColumnToggle is set to TRUE.
                                      # Needs to be done this way instead of
                                      # using uiOutput, otherwise the plot won't
                                      # render until the user manually goes to
                                      # the settings tab so the selectize input
                                      # initializes
                                      shiny::tabsetPanel(type = "hidden",
                                                         id = "switchColorCode",
                                                         shiny::tabPanelBody("showColorCodeColumnInput",
                                                                             tags$span(title = "Select column to use for color coding",
                                                                                       shiny::selectizeInput(
                                                                                         inputId = "colorCodeColumnInput",
                                                                                         label = NULL,
                                                                                         choices = "flagged_samples",
                                                                                         selected = "flagged_samples",
                                                                                         multiple = FALSE,
                                                                                         width = "100%"
                                                                                       ))),
                                                         shiny::tabPanelBody("hideColorCodeColumnInput", NULL)))
                      ),
                      shiny::column(width = 6,
                                    bslib::card(
                                      class = "h-100",
                                      title = "Instructions",
                                      shiny::markdown(
                                        mds = c(
                                          "## Selecting points",
                                          "",
                                          "Upon uploading a dataset to annotate, a new column (`keep_points`) will be added to the dataframe and filled with `TRUE`.",
                                          "",
                                          "The pitch pulses for a specified file will be plotted in the main plotting region.
        Points that you want to keep (`keep_pulse=TRUE`) will be shown in closed circles, while points you want to remove (`keep_pulse=TRUE`) will be replaced with an open triangle.
        You can use the `[Show Line]` toggle button to plot a line through all the retained points.",
        "",
        "To remove a point, click and drag to select the desired points, then click the `[Remove]` button.
        To keep a point, click the `[Keep]` Button.
        You can click the `[Toggle Pulse]` button to flip the values of the selecteed points (all removed points will be switched to keep, all kept values will be switched to remove).
        ")
                                      ),
                                    )
                      ),

      )),
    bslib::nav_panel(
      title = "Progress",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(""),
        bslib::card_body(
          shiny::actionButton(
            inputId = "refreshProgressButton",
            title = "Click to recompute summary statistics on this page",
            label = "Refresh Diagnostics"
          ),
          shiny::actionButton(
            inputId = "plotUneditedFilesButton",
            title = "Click to send all unedited files to the editor pane",
            label = "Send Unedited Files to Editor"
          ),
          "Percentage of pulses removed from current dataset:",
          shiny::verbatimTextOutput(outputId = "percentRemovedText"),
          "Average variance of sample-to-sample semitone differences (+/- 2 se, averaged across all files and normalized to sampling period)",
          shiny::verbatimTextOutput(outputId = "changeInVarianceOutput"),
          "Total proportion of files that have been mended:",
          shiny::verbatimTextOutput(outputId = "nEditedFilesText"),
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
            shiny::column(width = 12,
                          class = "h-100",
                          # bslib::card(

                          tags$span(title = "Click to browse for and upload a csv file",
                          shiny::fileInput("rawFileUpload",
                                    "Choose CSV File",
                                    multiple = FALSE,
                                    buttonLabel = tags$span(icon("file-csv"), "Upload"),
                                    accept = c(".csv", ".txt"))),
                          shiny::markdown(
                            mds = c("## Example Data",
                                    "",
                                    "Click the button below to load a demo file with 36 contours")
                          ),
                          shiny::actionButton(
                            inputId = "loadFileButton",
                            title = "Click to load example data",
                            # label = shiny::uiOutput(outputId = "loadFileButtonLabel"),
                            label = "Load Demo Data",
                            style = "margin-bottom:8px;",
                            class = "btn-primary",
                            icon = icon("file-import"),
                            # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                            # shiny::icon("upload")
                          ),
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
                      title = "Click to flag potential errors in loaded dataset",
                      icon = icon('flag'),
                      width = "100%",
                      label = "Flag Samples"
                    )
                    # )
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
                                         fileChecked = NULL,
                                         badges = NULL,
                                         notes = NULL)
    filesBrushed <- shiny::reactiveValues(filenames = NULL)
    uneditedFiles <- shiny::reactiveValues(filenames = NULL)
    lastTransformation <- shiny::reactiveValues(pulse_ids = NULL)
    loadedFile <- shiny::reactiveValues(data = NULL)
    transformedColumn <- shiny::reactiveValues(name = NULL)
    plotFlag <- shiny::reactiveValues(value = TRUE)


    # Default keybindings, see https://craig.is/killing/mice for valid keys
    # Here what we're doing is setting each key to a reactive that's also mapped
    # to a button on the visible UI. This way all we need to change is the
    # reactive value and both the button and keybinding will be changed.
    boundKeys <- #shiny::reactiveValues(
      list("f" = keyBindAction(togglePulses, "[F] Pressed (Toggle)"),
           "r" = keyBindAction(removePulses, "[R] Pressed (Remove)"),
           "e" = keyBindAction(keepPulses, "[E] Pressed (Keep)"),
           "s" = keyBindAction(toggleShowLine, "[S] Pressed (Show Line)"),
           "q" = keyBindAction(filenav$goToPreviousFile, "[Q] Pressed (Previous File)"),
           "w" = keyBindAction(filenav$goToNextFile, "[W] Pressed (Next File)"),
           "b" = keyBindAction(plotBrushed, "[B] Pressed (Plot Brushed)"),
           "d" = keyBindAction(doublePulses, "[D] Pressed (Double Pulses)"),
           "a" = keyBindAction(halvePulses, "[A] Pressed (Halve Pulses)"),
           "v" = keyBindAction(plotMatches, "[V] pressed (Plot Matches)"),
           "ctrl+z" = keyBindAction(undoTransformation, "[Ctrl+Z] Pressed (Undo Transform)"),
           "command+z" = keyBindAction(undoTransformation, "[Command+Z] Pressed (Undo Transform)"))
    # )




    observeEvent(input$keys, {
      # Key bindings only apply on the editor page
      if (input$navbar != "Editor" | (!is.null(input$useKeysToggle) && !input$useKeysToggle))
        return(NULL)

      # Call the appropriate reactive from the keybindings we set
      boundKeys[[input$keys]]()
    })


    getBrushedPoints <- shiny::reactive({
      yval <- transformedColumn$name
      if (!is.null(input$hideToggleInput) && input$hideToggleInput)
        yval <- input$yValColumnInput

      shiny::brushedPoints(plotSubset$data,
                           input$plot_brush,
                           xvar = input$xValColumnInput,
                           yvar = yval)

    })

    plotSubset <- reactiveValues(data = NULL)

    refilterSubset <- function(){
      if (is.null(loadedFile$data))
        return(NULL)

      message("Filtering")
      # browser()
      plotSubset$data <<- loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],]
      nPlotted$n <- sum(fileHandler$isPlotted)
      nPlotted$is_one <- nPlotted$n == 1
    }

    output$pulsePlot <- shiny::renderPlot({
      message('Rerendering')
      plotFlag$value
      if (is.null(loadedFile$data))
        return(NULL)

      # These will update when the user changes the color manually with the
      # color pickers or if the theme is changed.
      color_values <-  plotSettings$setColors[2:3]
      lineColor <- plotSettings$setColors[1]

      yval <- transformedColumn$name
      if (input$hideToggleInput)
        yval <- input$yValColumnInput

      # Set up the main aesthetics for the plot
      p <-
        ggplot2::ggplot(plotSubset$data,
                        ggplot2::aes(x = !!sym(input$xValColumnInput),
                                     y = !!sym(yval),
                                     group = !!sym(input$filenameColumnInput)))

      # Add the line if the user wants it
      if (plotSettings$showLine) {
        # If the user wants the line to go through the removed points,
        # then we don't need to subset the data to only the retained pulses
        if (input$useRemovedPointsToggleInput) {
          p <- p +
            ggplot2::geom_line(data =plotSubset$data, color = lineColor)
        } else {
          # use remove points toggle is off
          p <- p +
            ggplot2::geom_line(data =plotSubset$data[plotSubset$data[[input$selectionColumnInput]],], color = lineColor)
        }
      }

      # Add the points, if the user wants to use the flagged column, use it to color the points
      # otherwise just use the keep_pulse column to redundantly code that information
      if (input$useFlaggedColumnToggle && input$colorCodeColumnInput %in% colnames(plotSubset$data)) {
        p <- p +
          ggplot2::geom_point(ggplot2::aes(color = !!sym(input$colorCodeColumnInput),
                                           shape = !!sym(input$selectionColumnInput)),
                              size = input$sizeSlider,
                              alpha = input$alphaSlider)
      } else {
        p <- p +
          ggplot2::geom_point(ggplot2::aes(color = !!sym(input$selectionColumnInput),
                                           shape = !!sym(input$selectionColumnInput)),
                              size = input$sizeSlider,
                              alpha = input$alphaSlider)
      }

      # Color code logical values
      if ((!input$useFlaggedColumnToggle || is.logical(plotSubset$data[[input$colorCodeColumnInput]]))) {
        # Make sure the color order is correct for the TRUE and FALSE values if not using the color code column
        if (input$useFlaggedColumnToggle)
          color_values <- c(color_values[2], color_values[1])
        p <- p +
          ggplot2::scale_color_manual(values = color_values)
      }

      # Finish setting some of the theme options
      p <- p +
        ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 2)) +
        ggplot2::scale_y_continuous(limits = input$pitchRangeInput) +
        ggplot2::xlab(input$xValColumnInput) +
        ggplot2::ylab(input$yValColumnInput) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::theme(legend.position = 'top') +
        ggplot2::theme(panel.grid = ggplot2::element_line(linewidth = .3)) +
        plotSettings$themeColors +
        # If a single file is shown, use that file as the title, otherwise use "Multiple Files"
        ggplot2::ggtitle(label = ifelse(nPlotted$is_one,
                                        fileHandler$filenames[fileHandler$isPlotted],
                                        "Multiple Files"))


      p
    })

    togglePulses <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()
      print(input$pitchRangeInput)
      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, c(input$selectionColumnInput) := !get(input$selectionColumnInput)]
        plotSubset$data[plot_vals_to_change, c(input$selectionColumnInput) := !get(input$selectionColumnInput)]
        selectedPoints$data <- NULL
        updatePlot()
      }
    })

    # When the user clicks the toggle button, toggle the logical keep_pulse value
    shiny::observeEvent(input$toggleButton, {
      message("Toggle Pressed")
      togglePulses()
    })

    shiny::observeEvent(input$colorCodeColumnInput,ignoreInit = TRUE, {
      if (is.null(loadedFile$data) | is.null(input$colorCodeColumnInput) | is.null(input$useFlaggedColumnToggle))
        return(NULL)

      if (input$useFlaggedColumnToggle) {
        updatePlot()
      }
    })

    shiny::observeEvent(input$useFlaggedColumnToggle,ignoreInit = FALSE, {
      message("flag toggle")
      if (input$useFlaggedColumnToggle){
        shiny::updateTabsetPanel(inputId = "switchColorCode", selected = "showColorCodeColumnInput")
      } else {
        shiny::updateTabsetPanel(inputId = "switchColorCode", selected = "hideColorCodeColumnInput")
      }
      updatePlot()
    })


    # Toggle point on click
    shiny::observeEvent(input$plot_click,ignoreInit = TRUE, {
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
        loadedFile$data[first_id, c(input$selectionColumnInput) := !get(input$selectionColumnInput)]
        plot_vals_to_change <- plotSubset$data$pulse_id == first_id
        plotSubset$data[plot_vals_to_change, c(input$selectionColumnInput) := !get(input$selectionColumnInput)]
        clickedPoint <- NULL
        updatePlot()
      }
    })

    keepPulses <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, c(input$selectionColumnInput) := TRUE]
        plotSubset$data[plot_vals_to_change, c(input$selectionColumnInput) := TRUE]
        selectedPoints$data <- NULL

        updatePlot()

      }})

    removePulses <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      # Toggle the color of the selected points
      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, c(input$selectionColumnInput) := FALSE]
        plotSubset$data[plot_vals_to_change, c(input$selectionColumnInput) := FALSE]

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

        shiny::updateSelectizeInput(session,
                                    inputId = inputId,
                                    choices = colnames(data_ref$data),
                                    selected = ifelse(input_value %in% colnames(data_ref$data),
                                                      input_value,
                                                      colnames(data_ref$data)[1]))
      }
      )
    }

    shiny::observeEvent(input$yValColumnInput,ignoreInit = TRUE, {
      if (is.null(loadedFile$data))
        return(NULL)
      changeTransformedColumn()
    })

    output$cwd <- shiny::renderText({
      getwd()
    })

    shiny::observeEvent(input$rawFileUpload, {

      req(input$rawFileUpload)
    # shiny::observeEvent(input$loadFileButton, {
      message("Load File Pressed")

      file_to_load <- input$rawFileUpload$datapath
      message(paste0("Loading file ", file_to_load))
      loadedFile$data <- NULL # If previous data was loaded, throw it out
      loadedFile$data <- data.table::fread(file_to_load)  # Use fread from data.table package

      # Add animations to some of the important buttons
      shinyjs::addClass("web_fileNav-saveButton", class = "animbutton")
      shinyjs::addClass("checkVisibleFilesButton", class = "animbutton")

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

      data.table::setorderv(loadedFile$data, cols = c(input$filenameColumnInput, input$xValColumnInput))

      # if (!file.exists(outFile))
      loadedFile$data[, (input$selectionColumnInput) := where_not_zero(get(input$yValColumnInput))]

      if (!"pulse_id" %in% colnames(loadedFile$data))
        loadedFile$data[, pulse_id := .I]  # Use .I from data.table package

      if (!"pulse_transform" %in% colnames(loadedFile$data)){
        loadedFile$data[, pulse_transform := 1.0]
      }

      changeTransformedColumn()

      fileHandler$filenames <- unique(loadedFile$data[[input$filenameColumnInput]])
      fileHandler$isPlotted <- rep(TRUE, length(fileHandler$filenames))

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
        paste0("Working File:\n", basename(file_to_load))
      })

      # If we've loaded another file in the same session then we need
      # to reset flag samples button

      if ("flagged_samples" %in% colnames(loadedFile$data)) {
        shinyjs::addClass(id = "flagSamplesButton", class = "btn-success")
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
        shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      } else {
        shinyjs::removeClass(id = "flagSamplesButton", class = "btn-success")
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("flag"))
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, input$colorCodeColumnInput)()
      }

      if (input$useBadgesToggle) {
        if (!"tags" %in% colnames(loadedFile$data)) {
          loadedFile$data[, tags := NA_character_]
          fileHandler$badges <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$badges) <- fileHandler$filenames
        } else {
          fileHandler$badges = loadedFile$data[, .(tags = tags[1]), by = c(input$filenameColumnInput)][['tags']]
          names(fileHandler$badges) <- fileHandler$filenames
          loadedFile$data[,tags := as.character(tags)]
        }
      }

      if (input$useNotesToggle) {
        if (!"notes" %in% colnames(loadedFile$data)) {
          loadedFile$data[, notes := NA_character_]
          fileHandler$notes <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$notes) <- fileHandler$filenames
        } else {
          fileHandler$notes = loadedFile$data[, .(notes = notes[1]), by = c(input$filenameColumnInput)][['notes']]
          names(fileHandler$notes) <- fileHandler$filenames
          loadedFile$data[,notes := as.character(notes)]
        }
      }

      refilterSubset()
      updatePlot()

    })

    # observe({
    #   if (!is.null(input$fileSelectBox) && !is.null(input$inputDirInput)) {
    #     shinyjs::removeClass("loadFileButton", class = "btn-warning")
    #     shinyjs::addClass("loadFileButton", class = "btn-primary")
    #     shiny::updateActionButton(session, "loadFileButton",icon = icon("upload"))
    #   } else {
    #     shinyjs::removeClass("loadFileButton", class = "btn-primary")
    #     shinyjs::addClass("loadFileButton", class = "btn-warning")
    #     shiny::updateActionButton(session, "loadFileButton",icon = icon("spinner"))
    #   }
    # })

    shiny::observeEvent(input$loadFileButton, {
      message("Load demo file Pressed")

      file_to_load <- file.path(get_example_f0_data("."), "demo_data.csv")


      message(paste0("Loading file ", file_to_load))
      loadedFile$data <- NULL # If previous data was loaded, throw it out
      loadedFile$data <- data.table::fread(file_to_load)  # Use fread from data.table package

      # Upon successful load of the file, change the color of the load file button
      # so it doesn't stand out as much anymore
      shinyjs::removeClass("loadFileButton", "btn-primary")

      # Add animations to some of the important buttons
      shinyjs::addClass("fileNav-saveButton", class = "animbutton")
      shinyjs::addClass("checkVisibleFilesButton", class = "animbutton")

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

      data.table::setorderv(loadedFile$data, cols = c(input$filenameColumnInput, input$xValColumnInput))  # Use setorder from data.table package

      # if (!file.exists(outFile))
      if (!input$selectionColumnInput %in% colnames(loadedFile$data))
        loadedFile$data[, (input$selectionColumnInput) := where_not_zero(get(input$yValColumnInput))]  # Use := operator from data.table package

      if (!"pulse_id" %in% colnames(loadedFile$data))
        loadedFile$data[, pulse_id := .I]  # Use .I from data.table package

      if (!"pulse_transform" %in% colnames(loadedFile$data)){
        loadedFile$data[, pulse_transform := 1.0]
      }

      changeTransformedColumn()

      fileHandler$filenames <- unique(loadedFile$data[[input$filenameColumnInput]])
      fileHandler$isPlotted <- rep(TRUE, length(fileHandler$filenames))

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
        paste0("Working File:\n", clean_file(input$fileSelectBox))
      })

      # If we've loaded another file in the same session then we need
      # to reset flag samples button

      if ("flagged_samples" %in% colnames(loadedFile$data)) {
        shinyjs::addClass(id = "flagSamplesButton", class = "btn-success")
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
        shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      } else {
        shinyjs::removeClass(id = "flagSamplesButton", class = "btn-success")
        shiny::updateActionButton(session, "flagSamplesButton", icon = icon("flag"))
        set_selectize_choices(session, "colorCodeColumnInput", loadedFile, input$colorCodeColumnInput)()
      }

      if (input$useBadgesToggle) {
        if (!"tags" %in% colnames(loadedFile$data)) {
          loadedFile$data[, tags := NA_character_]
          fileHandler$badges <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$badges) <- fileHandler$filenames
        } else {
          fileHandler$badges = loadedFile$data[, .(tags = tags[1]), by = c(input$filenameColumnInput)][['tags']]
          names(fileHandler$badges) <- fileHandler$filenames
          loadedFile$data[,tags := as.character(tags)]
        }
      }

      if (input$useNotesToggle) {
        if (!"notes" %in% colnames(loadedFile$data)) {
          loadedFile$data[, notes := NA_character_]
          fileHandler$notes <- rep(NA_character_, length(fileHandler$filenames))
          names(fileHandler$notes) <- fileHandler$filenames
        } else {
          fileHandler$notes = loadedFile$data[, .(notes = notes[1]), by = c(input$filenameColumnInput)][['notes']]
          names(fileHandler$notes) <- fileHandler$filenames
          loadedFile$data[,notes := as.character(notes)]
        }
      }

      refilterSubset()
      updatePlot()

    })

    toggleShowLine <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      plotSettings$showLine <- !plotSettings$showLine
    })

    # Toggle whether the line should be shown or not
    shiny::observeEvent(input$showLineButton, {
      toggleShowLine()
    })




    nPlotted = shiny::reactiveValues(n = NULL,
                                     is_one = NULL)

    observe({
      nPlotted$n = sum(fileHandler$isPlotted)
      nPlotted$is_one = nPlotted$n == 1
    })

    # Display the number of files/contours that are currently plotted
    output$nFilesPlotted <- shiny::renderText({
      paste0("# of files shown: ", nPlotted$n)
    })

    # When the user clicks the plot matches button, plot the files that match the
    # regex in the filter regex box
    shiny::observeEvent(input$plotMatchesButton, {
      message("Plot Matches Pressed")
      plotMatches()
    })

    plotMatches <- reactive({
      print(input$filterRegex)

      # Check off the displayed file if we're only looking at 1
      if (nPlotted$is_one) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
        annotations$saveNotes()
        annotations$saveBadges()
      }

      selected_files <- grepl(input$filterRegex,fileHandler$filenames)

      if (any(selected_files)) {

        fileHandler$isPlotted <- selected_files

        refilterSubset()
        annotations$updateBadges()
        annotations$updateNotes()
        destroyLoadedAudio()
      }

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

    plotBrushed <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      annotations$saveNotes()
      annotations$saveBadges()

      selected_files <- fileHandler$filenames %in% filesBrushed$filenames

      # any short circuits on the first TRUE value
      if (any(selected_files)) {
        fileHandler$isPlotted <- fileHandler$filenames %in% filesBrushed$filenames
        message(paste0("Plotting ", sum(fileHandler$isPlotted), " files from selection"))

        refilterSubset()

        annotations$updateBadges()
        annotations$updateNotes()
        destroyLoadedAudio()
      }
    })

    # When the user clicks the go to brushed button, plot only the files that the
    # user selected in the editor
    shiny::observeEvent(input$goToBrushButton, {
      message("Go to brushed pressed")
      plotBrushed()
    })

    # Refresh the diagnostics pane when the user clicks the refresh button
    shiny::observeEvent(input$refreshProgressButton,{
      if (is.null(loadedFile$data))
        return(NULL)
      # browser()
      filtered_data <- loadedFile$data[where_not_zero(get(input$yValColumnInput)),]

      uneditedFiles$filenames <-
        filtered_data[, .(n_edited = any(!get(input$selectionColumnInput))), by = c(input$filenameColumnInput)][!(n_edited), .SD[[input$filenameColumnInput]]]


      output$percentRemovedText <- shiny::renderText({
        paste0(round(sum(!filtered_data[[input$selectionColumnInput]]) / nrow(filtered_data)*100, 2), "%")
      })

      output$nEditedFilesText <- shiny::renderText({
        n_files <- length(fileHandler$filenames)
        n_edited <- n_files - length(uneditedFiles$filenames)
        paste0(n_edited, " / ", n_files)
      })

      output$changeInVarianceOutput <- shiny::renderText({
        samplerate <- median(diff(loadedFile$data[[input$xValColumnInput]][1:100]))

        variance_values <-
          loadedFile$data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(input$filenameColumnInput))) |>

          dplyr::reframe(
            original_variance = .var_of_diffs(.data[[input$yValColumnInput]][where_not_zero(.data[[input$yValColumnInput]])],
                                              .data[[input$xValColumnInput]],
                                              samplerate),
            new_variance = .var_of_diffs(.data[[transformedColumn$name]][.data[[input$selectionColumnInput]] & where_not_zero(.data[[input$yValColumnInput]])],
                                         .data[[input$xValColumnInput]],
                                         samplerate)) |>
          dplyr::ungroup()

        mean_se1 <- ggplot2::mean_se(variance_values$original_variance, mult = 2)
        mean_se2 <- ggplot2::mean_se(variance_values$new_variance, mult = 2)

        paste0("Original Variance: ", round(mean_se1['y'], 3), "[", round(mean_se1['ymin'], 3), ",", round(mean_se1['ymax'], 3), "]\n",
               "New Variance: ", round(mean_se2['y'], 3), "[", round(mean_se2['ymin'], 3), ",", round(mean_se2['ymax'], 3), "]\n")

      })

      output$uneditedFilesText <- shiny::renderText({

        paste(uneditedFiles$filenames, collapse = "\n")
      })

    })

    plotSubsetFlag <- shiny::reactiveValues(value = FALSE)

    updatePlot <- shiny::reactive({
      # shiny::observe("keepFalseColor")
      plotFlag$value <- !plotFlag$value
    })


    doublePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
      plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id

      loadedFile$data[vals_to_change, pulse_transform := pulse_transform * 2.0]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]
      plotSubset$data[plot_vals_to_change, pulse_transform := pulse_transform * 2.0]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change

      current_pitch_range <- isolate(input$pitchRangeInput)
      max_transform_value <- max(loadedFile$data[[transformedColumn$name]])
      if (input$lockButton %% 2 == 0 && current_pitch_range[2L] < max_transform_value){
        new_pitch_max <-
          ceiling(add_semitones(max_transform_value, sign(max_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session, "pitchRangeInput",
                                              value = c(current_pitch_range[1L],
                                                        new_pitch_max))
      } else { # Otherwise the plot will render twice if the pitch range changes
        updatePlot()
      }

    })

    halvePulses <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()

      vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
      plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id

      # as.numeric needed to avoid implicit coercion to integer,
      # which turns the transformation from 0.5 to 0
      loadedFile$data[, pulse_transform:= as.numeric(pulse_transform)][vals_to_change, pulse_transform := pulse_transform * 0.5]
      loadedFile$data[vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]
      plotSubset$data[, pulse_transform:= as.numeric(pulse_transform)][plot_vals_to_change, pulse_transform := pulse_transform * 0.5]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

      selectedPoints$data <- NULL
      lastTransformation$pulse_ids <- vals_to_change

      current_pitch_range <- isolate(input$pitchRangeInput)
      min_transform_value <- min(loadedFile$data[[transformedColumn$name]][where_not_zero(loadedFile$data[[transformedColumn$name]])])
      if (input$lockButton %% 2 == 0 && current_pitch_range[1L] > min_transform_value){
        new_pitch_min <-
          ceiling(add_semitones(min_transform_value, sign(-min_transform_value)*1L))
        shinyWidgets::updateNumericRangeInput(session, "pitchRangeInput",
                                              value = c(new_pitch_min,
                                                        current_pitch_range[2L]))
      } else{
        updatePlot()
      }
    })


    # Multiply selected points by 2 (fixes halving errors)
    shiny::observeEvent(input$doubleButton, {
      message("Double Pressed")
      doublePulses()
    })

    changeTransformedColumn <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      # browser()
      transformedColumn$name <- paste(input$yValColumnInput, "transformed", sep = "_")
      new_values <- loadedFile$data[['pulse_transform']] * loadedFile$data[[input$yValColumnInput]]
      loadedFile$data[, (transformedColumn$name) := new_values]
      refilterSubset()
      # plotSubset$data[, (transformedColumn$name) := pulse_transform * get(input$yValColumnInput)]
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
      halvePulses()
    })

    undoTransformation <- reactive({
      if (is.null(loadedFile$data) || is.null(lastTransformation$pulse_ids))
        return(NULL)

      plot_vals_to_change <- plotSubset$data$pulse_id %in% loadedFile$data$pulse_id[lastTransformation$pulse_ids]

      loadedFile$data[lastTransformation$pulse_ids, pulse_transform := 1.0]
      loadedFile$data[lastTransformation$pulse_ids, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]
      plotSubset$data[plot_vals_to_change, pulse_transform := 1.0]
      plotSubset$data[plot_vals_to_change, c(transformedColumn$name) := get(input$yValColumnInput) * pulse_transform]

      lastTransformation$pulse_ids <- NULL
      selectedPoints$data <- NULL

      updatePlot()
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
      if(is.null(loadedFile$data) || is.null(uneditedFiles$filenames))
        return(NULL)

      fileHandler$isPlotted <- fileHandler$filenames %in% uneditedFiles$filenames
      refilterSubset()
      updatePlot()
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



    defaultPitchRange <- shiny::reactiveValues(min = 100, max = 500)

    output$pitchRangeUI <- shiny::renderUI({
      pitch_range <- c(100,500)
      one_st_step <- ceiling(add_semitones(pitch_range[2], 1) - pitch_range[1])

      if (!is.null(loadedFile$data)){
        pitch_range <- range(loadedFile$data[[transformedColumn$name]][where_not_zero(loadedFile$data[[transformedColumn$name]])])
        one_st_step <- ceiling(add_semitones(pitch_range[2], 1) - pitch_range[2])
        pitch_range <- floor(c(add_semitones(pitch_range[1], -1),
                               add_semitones(pitch_range[2], 1)))
        defaultPitchRange$min <- pitch_range[1]
        defaultPitchRange$max <- pitch_range[2]
      }
      tags$span(title = "Enter values to use as y-axis limits",
                shinyWidgets::numericRangeInput("pitchRangeInput",
                                                label =
                                                  tags$span(style = "display:inline-block",
                                                            span(id = "resetPitchRange",
                                                                 title = "Click to reset to default",
                                                                 # style = "cursor:pointer",
                                                                 style = "cursor:pointer;display:block;width:110px;float:left;margin-bottom:-8px;margin-right:109px",
                                                                 HTML(paste0(icon("clock-rotate-left"), " Pitch Range"))),
                                                            shiny::actionButton(
                                                              inputId = ("lockButton"),
                                                              label = NULL,
                                                              style  = "cursor:pointer;width:16px;display:block;float:right;margin-bottom:-8px;margin-left:8px;padding:0px",
                                                              title = "Click to lock pitch range and prevent dynamic changes",
                                                              icon = shiny::icon("lock-open"),
                                                              class = "showStopButton"
                                                            )
                                                  ),
                                                value = pitch_range,
                                                min = ifelse(pitch_range[1] > 0, 0, floor(add_semitones(pitch_range[1], sign(-pitch_range[1])*8))),

                                                max = ceiling(add_semitones(pitch_range[2], sign(pitch_range[1])*24)),
                                                step = one_st_step,
                                                width = "100%"))
    })

    observeEvent(input$lockButton, {
      if (input$lockButton %% 2 == 0) {

        shiny::updateActionButton(session, "lockButton",
                                  icon = shiny::icon("lock-open"))
        shinyjs::enable(selector = "#pitchRangeInput .input-numeric-range")
      } else {
        shiny::updateActionButton(session, "lockButton",
                                  icon = shiny::icon("lock"))
        shinyjs::disable(selector = "#pitchRangeInput .input-numeric-range")
      }
    })

    shinyjs::onclick("resetPitchRange", {
      shinyWidgets::updateNumericRangeInput(session, "pitchRangeInput",
                                            value = c(defaultPitchRange$min, defaultPitchRange$max))
    })

    # The unedited and edited selectInput boxes' default behavior will change
    # the plotted file to whatever is selected (=is displayed in the box). This
    # behavior needs to be suppressed since the choices dynamically change as
    # the user checks files. This is done by using shinyjs::onevent to only change
    # the plotted file when the user clicks on an option.
    shinyjs::onevent(id = "uneditedFileSelectUI",event = "change",
                     expr= {
                       if (!is.null(input$uneditedFileSelectBox) && !identical(input$uneditedFileSelectBox, character(0))) {
                         if (nPlotted$is_one)
                           fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
                         annotations$saveBadges()
                         annotations$saveNotes()
                         fileHandler$isPlotted[] <- FALSE
                         fileHandler$isPlotted[fileHandler$filenames == input$uneditedFileSelectBox] <- TRUE
                         annotations$updateBadges()
                         annotations$updateNotes()
                         refilterSubset()
                         updatePlot()
                         # updatePlotSettingsData()
                       }
                     })

    shinyjs::onevent(id = "editedFileSelectUI",event = "change",
                     expr= {
                       if (!is.null(input$editedFileSelectBox) && !identical(input$editedFileSelectBox, character(0))) {
                         if (nPlotted$is_one)
                           fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
                         annotations$saveBadges()
                         annotations$saveNotes()
                         fileHandler$isPlotted[] <- FALSE
                         fileHandler$isPlotted[fileHandler$filenames == input$editedFileSelectBox] <- TRUE
                         annotations$updateBadges()
                         annotations$updateNotes()
                         refilterSubset()
                         updatePlot()
                       }
                     })



    currentWave <- shiny::reactiveValues(value = NULL,
                                         path = NULL,
                                         exists = NULL,
                                         instance = NULL)

    destroyLoadedAudio <- function(id = "playAudio"){
      if (!is.null(currentWave$instance))
        audio::close.audioInstance(currentWave$instance)
      ns <- NS(id)

      shinyjs::addClass(ns("stopButton"), "hideStopButton",asis = TRUE)
      shinyjs::addClass(ns("playVisibleFile"), "bigPlayButton",asis = TRUE)
      shinyjs::removeClass(ns("playVisibleFile"), "smallPlayButton",asis = TRUE)
      shinyjs::removeClass(ns("stopButton"),
                           "showStopButton",asis = TRUE)

      currentWave$instance <<- NULL
      currentWave$value <<- NULL
      currentWave$path <<- NULL
      currentWave$exists <<- NULL
    }

    shinyjs::onclick(id = "keysQuestion", {
      shinyWidgets::show_alert(

        title = "Keyboard Shortcuts",
        type = 'info',
        width = "35em",
        html = TRUE,
        text = tagList(
          tags$div(style = css(`text-align` = "left",
                               `max-height` = "400px",
                               `overflow-y` = "scroll"),
                   shiny::markdown(
                     mds = c(
                       "If keyboard shortcuts are turned on, you can use the following on the Editor page:",
                       inline_kbd_button('f', " - {KEY}: Toggle pulses"),
                       inline_kbd_button('r', " - {KEY}: Remove pulses"),
                       inline_kbd_button('e', " - {KEY}: Keep pulses"),
                       inline_kbd_button('s', " - {KEY}: Show/Hide Line"),
                       inline_kbd_button('q', " - {KEY}: Go to Previous File"),
                       inline_kbd_button('w', " - {KEY}: Go to Next File"),
                       inline_kbd_button('b', " - {KEY}: Plot brushed files"),
                       inline_kbd_button('d', " - {KEY}: Double selected pulses"),
                       inline_kbd_button('a', " - {KEY}: Halve selected pulses"),
                       inline_kbd_button('v', " - {KEY}: Plot files matching regex"),
                       inline_kbd_button('p', " - {KEY}: Clear Praat objects"),
                       inline_kbd_button(c("ctrl", "z"), " - {KEY}: Undo last transform")
                     )),
                   tags$p(style = css(`font-size` = ".85em",
                                      `margin-bottom` = "none"),
                          icon("heart"), "made with ",
                          tags$a(href = "https://shhdharmen.github.io/keyboard-css/", "keyboard-css"),
                          " and ",
                          tags$a(href = "https://github.com/r4fun/keys", "{keys}"))),
        ))

    })

    # When the user clicks the Check off Files button, all files currently displayed
    # will have their fileChecked values set to TRUE.
    shiny::observeEvent(input$checkVisibleFilesButton, {
      message("Check visible files pressed")
      if (!is.null(fileHandler$filenames) && any(fileHandler$isPlotted)) {
        fileHandler$fileChecked[fileHandler$isPlotted] <- TRUE
      }
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
                              .unique_file = input$filenameColumnInput,
                              .hz = input$yValColumnInput,
                              .time = input$xValColumnInput,
                              .samplerate = NA,
                              .speaker = "Speaker", # change this later
                              .as_vec = TRUE)

      loadedFile$data[, ("flagged_samples") := flagged_values]

      if (!data.table::is.data.table(loadedFile$data))
        loadedFile$data <- data.table(loadedFile$data)

      shinyjs::addClass(id = 'flagSamplesButton',class = "btn-success")
      shinyjs::removeClass("flagSamplesButton", class = "btn-warning")
      shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
      shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
      set_selectize_choices(session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      refilterSubset()

    })

    # Note: the input is created from within the window resizer module via
    #       javascript, so it's actually visible from this scope and needs
    #       to be passed into the module server function like so
    windowResizeServer('windowListener', reactive(input$windowChange))


    colorServer("colors",
                plotSettings,
                updatePlot,
                reactive(input$dark_mode))

    annotations <- annotationServer("annotations",
                                    loadedFile,
                                    fileHandler,
                                    updatePlot,
                                    reactive(input$filenameColumnInput),
                                    reactive(input$useNotesToggle),
                                    reactive(input$useBadgesToggle),
                                    nPlotted)


    filenav <- web_fileNavServer("web_fileNav",
                             loadedFile,
                             fileHandler,
                             reactive(input$skipCheckedFilesToggle),
                             reactive(input$filenameColumnInput),
                             nPlotted,
                             annotations,
                             refilterSubset,
                             destroyLoadedAudio)



  }

  shiny::shinyApp(ui, server)
}


