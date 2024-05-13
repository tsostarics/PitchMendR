#' Run pulse editor GUI
#'
#' Runs the shiny app
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
#' @param yvar Y variable to use, should exist in the file to be loaded. Default
#' is `'f0'`.
#' @param xvar X variable touse, should exist in the file to be loaded. Default
#' is `'t_ms'`.
#'
#' @return Nothing
#' @export
#'
#' @importFrom shiny tags isolate reactive moduleServer observeEvent icon req reactiveValues observe
#' @importFrom rlang sym
#' @importFrom stats median
openEditor <- function(
    input_directory = get_example_f0_data("."),
    output_directory = "./",
    audio_directory = "./audio",
    textgrid_directory = "./audio",
    praat_path = "./Praat.exe",
    yvar = "f0",
    xvar = "t_ms",
    ...) {
  ui <- bslib::page_navbar(

    id = "navbar",
    title = "PitchMendR",
    selected = "Setup",
    collapsible = TRUE,
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      gap = ".75rem",
      title = tags$header("Tools", class = "sidebar-title", style = "margin-bottom: .25rem; padding-bottom: .5rem"),
      shinyjs::useShinyjs(), # Placed here to avoid a warning if placed above a tab
      keys::useKeys(),
      use_keyboardcss(),
      singleton(
        htmltools::includeCSS(
          system.file("cssfiles/button_animation.css", package = "PitchMendR")
        )),
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
                                      "p",
                                      "o",
                                      "j")
      ),
      tags$span(title = "Click to show/hide contour lines",
                shinyWidgets::awesomeCheckbox(
                  inputId = "hideLineButton",
                  label = "Hide contour line",
                  value = FALSE,
                  status = "info")),
      tags$span(title = "Click to show/hide points",
                shinyWidgets::awesomeCheckbox(
                  inputId = "hidePointsButton",
                  label = "Hide points",
                  value = FALSE,
                  status = "info")),
      tags$span(title = "Toggle to hide doubling/halving transforms",
                shinyWidgets::awesomeCheckbox(inputId = "hideToggleInput",
                                              label= "Hide transform",
                                              value = FALSE,
                                              status = "info")),
      tags$span(title = "Toggle to draw line through removed points",
                shinyWidgets::awesomeCheckbox(inputId = "useRemovedPointsToggleInput",
                                              label= "Show \u25B3s in line",
                                              value = FALSE,
                                              status = "info")),
      shiny::actionButton("clearSelectButton",
                          "Clear Selection"),
      undoTransformUI('octaveShift'),
      shiny::actionButton(
        inputId = "checkVisibleFilesButton",
        title = "Click to check off currently plotted files",
        icon = shiny::icon('check'),
        label = "off visible"
      ),
      praatUI_button("praatIO"),
      playAudioUI("playAudio"),
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
        margin-bottom: 0px;
      }
      .compressed-sidebar-title {
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
          # tags$head(
          #  tags$style(HTML("
          #    #pitchRangeInput-label.control-label {
          #      width: 100px;
          #    }
          #    "
          #  ))
          # ),
          bslib::card( height = "88vh",
                       title = "Plot Settings",
                       loadFile_workingFileOutput('loadFile'),
                       # shiny::textOutput(outputId = "workingFileOutput"),
                       shiny::uiOutput(outputId = "pitchRangeUI"),
                       tags$span(title = "Horizontal zoom in/out", "Timespan Controls"),
                       tags$span(style = "display:inline-flex;flex-direction: row; justify-content: space-between;",
                                 shiny::actionButton(inputId = "xZoomAllButton",
                                                     label = "ALL",
                                                     style = "padding-left: 0px;padding-right:0px;padding-top:4px;padding-bottom:4px;",
                                                     title = "Zoom out to show entire contour(s)",
                                                     width= "22%"),
                                 shiny::actionButton(inputId = "xZoomInButton",
                                                     label = "IN",
                                                     style = "padding-left: 0px;padding-right:0px;padding-top:4px;padding-bottom:4px;",
                                                     title = "Zoom in",
                                                     width= "22%"),
                                 shiny::actionButton(inputId = "xZoomOutButton",
                                                     label = "OUT",
                                                     style = "padding-left: 0px;padding-right:0px;padding-top:4px;padding-bottom:4px;",
                                                     title = "Zoom out",
                                                     width= "22%"),
                                 shiny::actionButton(inputId = "xZoomSelButton",
                                                     label = "SEL",
                                                     style = "padding-left: 0px;padding-right:0px;padding-top:4px;padding-bottom:4px;",
                                                     title = "Zoom to selection",
                                                     width= "22%")),
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
            shinyjqui::jqui_resizable(


              shiny::plotOutput(outputId = "pulsePlot",
                                click = "plot_click",
                                brush = "plot_brush",
                                height = "100%"),
              options = list(containment = "parent",
                             save = TRUE)),
            shiny::uiOutput("brushToolTip",inline = TRUE),

            fill = TRUE,
            height="88vh",
            width = '80vw',
            # shiny::fluidRow(#style = "height:20vh;width:90vw",
            # Adjust the gap space between the buttons
            tags$head(tags$style(shiny::HTML(".bslib-gap-spacing { gap: 8px; } "))),
            bslib::layout_columns(height = "20%",width = '80vw',fillable = TRUE,id = "controlButtons",
                                  bslib::card(fill = TRUE,
                                              fileNavUI("fileNav"),
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
                                              octaveShiftUI('octaveShift')
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
                                                                      choices = xvar,
                                                                      selected = xvar,
                                                                      multiple = FALSE,
                                                                      width = "100%")),
                                      tags$span(title = "Select column that identifies the y-axis for the plot",
                                                shiny::selectizeInput("yValColumnInput",
                                                                      label ="Y-value column name",
                                                                      choices = yvar,
                                                                      selected = yvar,
                                                                      multiple = FALSE,
                                                                      width = "100%")),
                                      submitTextInput("selectionColumnInput",
                                                      title = "Click button set column name",
                                                      label = "Column name for keep/remove annotations (will be added if it doesn't exist)",
                                                      value = "keep_pulse",
                                                      width = "100%"),
                                      shiny::markdown(mds = "## UI Options"),
                                      tags$span(title = "Toggle to save file to disk when plot refreshes with new files",
                                                shinyWidgets::awesomeCheckbox(
                                                  inputId = "saveOptionButton",
                                                  label = "Save on file navigation (disable to improve responsiveness)",
                                                  value = FALSE,width = "100%",
                                                  status = "info"
                                                )),
                                      tags$span(title = "Toggle to skip checked files when using previous/next file buttons",
                                                shinyWidgets::awesomeCheckbox(
                                                  inputId = "skipCheckedFilesToggle",
                                                  label = "Skip checked files on file navigation",
                                                  value = FALSE,
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
                                      "Override plot aesthetics below:",
                                      colorUI("colors"),
                                      tags$span(title = "Toggle to use column in dataset for color coding",
                                                shinyWidgets::materialSwitch(
                                                  inputId = "useFlaggedColumnToggle",
                                                  label="Color points by column:",
                                                  value = FALSE,
                                                  inline = TRUE,
                                                  status = "info")),
                                      tags$span(title = "Drag to change size of points",
                                                shiny::sliderInput("sizeSlider", "Point Size", min = 1, max = 10, value = 2)),
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
                      shiny::column(width = 6, columnInfo_UI("columnInfo")),

      )),
    diagnosticsUI('diagnostics'),
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
                          class = "h-100",
                          # bslib::card(height = "100%",
                          title = "Directory Settings",
                          "Current working directory:",
                          shiny::verbatimTextOutput(outputId = "cwd"),
                          tags$span(title = "Enter the directory containing spreadsheet to load",
                                    shiny::textInput(
                                      inputId = "inputDirInput",
                                      label =  "Dataset input directory",
                                      value = input_directory,
                                      width = "100%"
                                    )),
                          tags$span(title = "Enter the directory to save annotated spreadsheet to",
                                    shiny::textInput(
                                      inputId = "outputDirInput",
                                      label =  "Output directory for annotated datasets",
                                      value = output_directory,
                                      width = "100%"
                                    )),
                          tags$span(title = "Select a file to load from the input directory",
                                    shiny::selectizeInput(inputId ="fileSelectBox",
                                                          label =  "Files Available (*=not processed yet)",
                                                          multiple = FALSE,
                                                          choices = NULL)),
                          loadFileUI('loadFile'),
                          "",
                          tags$span(title = "Click to expand audio options",
                                    praatUI_input("praatIO", praat_path, audio_directory, textgrid_directory))
                          # )
            ),
            shiny::column(width = 6,
                          class = "h-100",
                          # bslib::card(
                          title = "Flagging Samples",
                          shiny::markdown(
                            mds = c(
                              "## Flagging samples",
                              "",
                              "Click the button below to automatically annotate potential F0 tracking errors as regions of interest.",
                              "Note that this is not perfect and may flag samples that are not tracking errors and miss samples that are tracking errors.",
                              "",
                              "If the column `F0_semitones` already exists, it will be used to identify errors.
                    If not, this column will be added by computing semitones from the speaker's median pitch.
                    Check the settings tab for the column names used for the time, pitch, and filename values.",
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
    ),
    bslib::nav_panel(
      title = "How-to",
      shiny::column(width = 7,
                    howto_UI("howto", TRUE)
      )
    ))


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
                                         notes = NULL,
                                         indices = NULL)
    filesBrushed <- shiny::reactiveValues(filenames = NULL)
    # uneditedFiles <- shiny::reactiveValues(filenames = NULL)
    lastTransformation <- shiny::reactiveValues(pulse_ids = NULL)
    loadedFile <- shiny::reactiveValues(data = NULL)
    transformedColumn <- shiny::reactiveValues(name = NULL)
    plotFlag <- shiny::reactiveValues(value = TRUE)

    # Default keybindings, see https://craig.is/killing/mice for valid keys
    # Here what we're doing is setting each key to a reactive that's also mapped
    # to a button on the visible UI. This way all we need to change is the
    # reactive value and both the button and keybinding will be changed.
    boundKeys <-
      list(
        "f" = keyBindAction(togglePulses,   "[F] Pressed (Toggle)"),
        "r" = keyBindAction(removePulses,   "[R] Pressed (Remove)"),
        "e" = keyBindAction(keepPulses,     "[E] Pressed (Keep)"),
        "s" = keyBindAction(toggleShowLine, "[S] Pressed (Show Line)"),
        "q" = keyBindAction(filenav$goToPreviousFile, "[Q] Pressed (Previous File)"),
        "w" = keyBindAction(filenav$goToNextFile,     "[W] Pressed (Next File)"),
        "b" = keyBindAction(plotBrushed,              "[B] Pressed (Plot Brushed)"),
        "d" = keyBindAction(octaveShift$doublePulses, "[D] Pressed (Double Pulses)"),
        "a" = keyBindAction(octaveShift$halvePulses,  "[A] Pressed (Halve Pulses)"),
        "v" = keyBindAction(plotMatches,              "[V] pressed (Plot Matches)"),
        "ctrl+z" = keyBindAction(octaveShift$undoTransformation,    "[Ctrl+Z] Pressed (Undo Transform)"),
        "command+z" = keyBindAction(octaveShift$undoTransformation, "[Command+Z] Pressed (Undo Transform)"),
        "p" = keyBindAction(audioInfo$closePraatFiles(), "[P] Pressed (Clear Praat Objects)"),
        "o" = keyBindAction(audioInfo$sendToPraat(), "[O] Pressed (Send to Praat)"),
        "j" = keyBindAction(audioServer$playAudio, "[J] Pressed (Play Audio Selection)")
      )

    observeEvent(input$keys, {
      # Key bindings only apply on the editor page
      if (input$navbar != "Editor" | (!is.null(input$useKeysToggle) && !input$useKeysToggle))
        return(NULL)
      # Call the appropriate reactive from the keybindings we set
      boundKeys[[input$keys]]()
    })

    observeEvent(input$clearSelectButton, {
      session$resetBrush('plot_brush')
    })

    selectionColumn <- shiny::reactiveVal()
    observeEvent(input$selectionColumnInputButton, {
      selectionColumn(isolate(input$selectionColumnInput))

      if (!is.null(loadedFile$data) && !selectionColumn() %in% colnames(loadedFile$data)) {
          loadedFile$data[, (selectionColumn()) := where_not_zero(get(input$yValColumnInput))]
          refilterSubset()
      }
    })

    ########################################################
    # Handle the x-axis zooming functionality

    horiz_bounds <- shiny::reactiveValues(xlim = NULL, full = NULL)

    # Change the horizontal bounds whenever we change the x variable to use
    observeEvent(input$xValColumnInput, {
      if(is.null(plotSubset$data))
        return(NULL)

      horiz_bounds$full <- suppressWarnings(range(plotSubset$data[[input$xValColumnInput]]))
      horiz_bounds$xlim <- horiz_bounds$full
    })

    observeEvent(input$xZoomInButton, {
      if(is.null(plotSubset$data))
        return(horiz_bounds$xlim)

      horiz_bounds$xlim <- zoom_by(horiz_bounds$xlim[1L], horiz_bounds$xlim[2L], factor = 0.5)
    })

    observeEvent(input$xZoomOutButton, {
      if(is.null(plotSubset$data))
        return(horiz_bounds$xlim)

      horiz_bounds$xlim <- zoom_by(horiz_bounds$xlim[1L], horiz_bounds$xlim[2L], factor = 2)
    })

    observeEvent(input$xZoomAllButton, {
      if(is.null(plotSubset$data))
        return(horiz_bounds$xlim)

      horiz_bounds$xlim <- horiz_bounds$full
    })

    observeEvent(input$xZoomSelButton, {
      if(is.null(plotSubset$data) | is.null(input$plot_brush))
        return(horiz_bounds$xlim)

      horiz_bounds$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    })
    ########################################################

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
    getIndices <- reactive({
      unlist(fileHandler$indices[fileHandler$isPlotted], recursive = FALSE, use.names = FALSE)
    })

    refilterSubset <- function(){
      if (is.null(loadedFile$data))
        return(NULL)

      # message("Filtering")

      plotted_indices <- getIndices()
      plotSubset$data <<- loadedFile$data[plotted_indices,]
      nPlotted$n <- sum(fileHandler$isPlotted)
      nPlotted$is_one <- nPlotted$n == 1
      horiz_bounds$full <<- suppressWarnings(range(plotSubset$data[[input$xValColumnInput]]))
      horiz_bounds$xlim <<- horiz_bounds$full
    }

    plot_colorColumn <- reactive({
      if(is.null(input$useFlaggedColumnToggle))
        return(NULL)
      req(selectionColumn())
      req(input$colorCodeColumnInput)
      req(loadedFile)
      colorColumn <- selectionColumn()
      if (input$useFlaggedColumnToggle && input$colorCodeColumnInput %in% colnames(loadedFile$data))
        colorColumn <- input$colorCodeColumnInput

      colorColumn
    })


    plot_yval <- reactive({
      if(is.null(input$hideToggleInput))
        return(NULL)

      req(input$yValColumnInput)
      req(transformedColumn$name)

      if (input$hideToggleInput)
        return(input$yValColumnInput)

      return(transformedColumn$name)
    })


    # Reactive to keep track of the current point color coding
    plot_colorCodePoints <- reactive({
      if(is.null(input$useFlaggedColumnToggle))
        return(NULL)
      req(input$colorCodeColumnInput)
      req(loadedFile)
      req(plotSettings)

      color_values <-  plotSettings$setColors[2:3]

      if ((!input$useFlaggedColumnToggle || is.logical(loadedFile$data[[input$colorCodeColumnInput]]))) {
        # Make sure the color order is correct for the TRUE and FALSE values if not using the color code column
        if (input$useFlaggedColumnToggle)
          color_values <- c(color_values[2], color_values[1])
        return(list(ggplot2::scale_color_manual(values = color_values)))
      }
      NULL
    })

    # Reactive for the color coding and plot theme, which should not change
    # very frequently.
    plot_addStyling <- reactive({
      req(input$pitchRangeInput)
      req(plotSettings)
      list(
        plot_colorCodePoints(),
        ggplot2::coord_cartesian(ylim = input$pitchRangeInput,
                                 xlim = horiz_bounds$xlim),
        ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 2)),
        ggplot2::theme_bw(base_size = 16),
        plotSettings$themeColors
      )

    })

    # If the points are hidden, then a dummy set of invisible points are going
    # to be plotted instead so that the legend still exists. But, we make all
    # of the legend aesthetics transparent. This is done to ensure the plot
    # height doesn't change when the points disappear.
    hide_legend_if_needed <- reactive({
      if (input$hidePointsButton)
        return(list(ggplot2::theme(legend.key        = element_rect(fill  = "#ffffff00"),
                                   legend.text       = element_text(color = "#ffffff00"),
                                   legend.title      = element_text(color = "#ffffff00"),
                                   legend.background = element_rect(fill  = "#ffffff00"))))

      NULL
    })


    output$pulsePlot <- shiny::renderPlot({
      # message('Rerendering')
      plotFlag$value
      if (is.null(loadedFile$data))
        return(NULL)


      # These will update when the user changes the color manually with the
      # color pickers or if the theme is changed.
      lineColor <- plotSettings$setColors[1]

      plot_line <- NULL
      if (plotSettings$showLine) {
        if (input$useRemovedPointsToggleInput) {
          plot_line <- list(ggplot2::geom_line(data =plotSubset$data,
                                               color = lineColor))
        } else {
          # browser()
          plot_line <- list(ggplot2::geom_line(data =plotSubset$data[plotSubset$data[[selectionColumn()]],],
                                               color = lineColor))
        }
      }

      # If we hide the points, we run into an issue where there are no longer
      # any variables mapped to a varying aesthetic, which makes the legend
      # disappear. Rather than somehow computing the space that the legend
      # normally takes up and adjusting the plot margin accordingly to leave
      # a placeholder space, I'm just going to plot an invisible point from the
      # data, which will create the legend, then make all the legend colors
      # invisible (see hide_legend_if_needed()) so that it takes up the space
      # but doesn't show any information.
      plot_points <-
        list(
          ggplot2::geom_point(data = plotSubset$data[1,],
                              # Using shape so the scale_shape_manual doesn't throw a warning
                              aes(shape = !!sym(selectionColumn())),
                              alpha = 0))

      if (!input$hidePointsButton){
        plot_points <- ggplot2::geom_point(aes(color = !!sym(plot_colorColumn()),
                                               shape = !!sym(selectionColumn())),
                                           size = input$sizeSlider)
      }

      # Set up the main aesthetics for the plot
      ggplot2::ggplot(plotSubset$data,
                      ggplot2::aes(x = !!sym(input$xValColumnInput),
                                   y = !!sym(plot_yval()),
                                   group = !!sym(input$filenameColumnInput)))+
        plot_line +
        plot_points +
        plot_addStyling() +
        hide_legend_if_needed() +
        # If a single file is shown, use that file as the title, otherwise use "Multiple Files"
        ggplot2::labs(x = input$xValColumnInput,
                      y = input$yValColumnInput,
                      title = ifelse(nPlotted$is_one,
                                     fileHandler$filenames[fileHandler$isPlotted],
                                     "Multiple Files"))
    })

    togglePulses <- reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      selectedPoints$data <- getBrushedPoints()
      if (!is.null(selectedPoints$data)) {
        vals_to_change <- loadedFile$data$pulse_id %in% selectedPoints$data$pulse_id
        plot_vals_to_change <- plotSubset$data$pulse_id %in% selectedPoints$data$pulse_id
        loadedFile$data[vals_to_change, c(selectionColumn()) := !get(selectionColumn())]
        plotSubset$data[plot_vals_to_change, c(selectionColumn()) := !get(selectionColumn())]
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
      # message("flag toggle")
      if (input$useFlaggedColumnToggle){
        shiny::updateTabsetPanel(inputId = "switchColorCode", selected = "showColorCodeColumnInput")
      } else {
        shiny::updateTabsetPanel(inputId = "switchColorCode", selected = "hideColorCodeColumnInput")
      }
      updatePlot()
    })

    # Renders a tool tip for the horizontal bounds of the selection on top of
    # the plot based on the coordinates from plot_brush
    output$brushToolTip <- shiny::renderUI( {
      if (is.null(loadedFile$data) | is.null(input$plot_brush))
        return(NULL)

      brush <- input$plot_brush
      xmin <- round(brush$xmin, 2)
      xmax <- round(brush$xmax, 2)

      left_px  <- brush$coords_css$xmin - 20
      right_px <- brush$coords_css$xmax + 2
      top_px   <- brush$coords_css$ymin

      const_style <- ";padding:2px;border:none;background-color:var(--bs-card-bg);opacity:.85;"

      left_style  <- paste0("position:absolute; left:", left_px, "px; top:", top_px, "px;text-align:right;", const_style)
      right_style <- paste0("position:absolute; left:", right_px,"px; top:", top_px, "px;margin-left:15px;", const_style)

      list(shiny::wellPanel(style = left_style,  shiny::p(HTML(paste0("<b>", xmin, "</b><br/>")))),
           shiny::wellPanel(style = right_style, shiny::p(HTML(paste0("<b>", xmax, "</b><br/>")))))

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
        loadedFile$data[first_id, c(selectionColumn()) := !get(selectionColumn())]
        plot_vals_to_change <- plotSubset$data$pulse_id == first_id
        plotSubset$data[plot_vals_to_change, c(selectionColumn()) := !get(selectionColumn())]
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
        loadedFile$data[vals_to_change, c(selectionColumn()) := TRUE]
        plotSubset$data[plot_vals_to_change, c(selectionColumn()) := TRUE]
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
        loadedFile$data[vals_to_change, c(selectionColumn()) := FALSE]
        plotSubset$data[plot_vals_to_change, c(selectionColumn()) := FALSE]

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

    shiny::observeEvent(input$yValColumnInput,ignoreInit = TRUE, {
      if (is.null(loadedFile$data))
        return(NULL)
      changeTransformedColumn()
    })

    output$cwd <- shiny::renderText({
      getwd()
    })

    loadFile <- loadFileServer("loadFile",
                               parent_session = session,
                               loadedFile,
                               reactive(input$fileSelectBox),
                               reactive(input$inputDirInput),
                               reactive(input$outputDirInput),
                               reactive(input$filenameColumnInput),
                               reactive(input$xValColumnInput),
                               reactive(input$yValColumnInput),
                               selectionColumn,
                               reactive(input$colorCodeColumnInput),
                               reactive(input$useBadgesToggle),
                               reactive(input$useNotesToggle),
                               changeTransformedColumn,
                               fileHandler,
                               plotSettings,
                               refilterSubset,
                               updatePlot)


    toggleShowLine <- reactive({
      shinyWidgets::updateAwesomeCheckbox(session, "hideLineButton", value = !input$hideLineButton)
    })

    # Toggle whether the line should be shown or not
    shiny::observeEvent(input$hideLineButton, {
      if (is.null(loadedFile$data))
        return(NULL)
      plotSettings$showLine <- !plotSettings$showLine
    })

    nPlotted = shiny::reactiveValues(n = NULL, is_one = NULL)

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


    updatePlot <- shiny::reactive({
      plotFlag$value <- !plotFlag$value
    })

    changeTransformedColumn <- shiny::reactive({
      if (is.null(loadedFile$data))
        return(NULL)
      transformedColumn$name <- paste(input$yValColumnInput, "transformed", sep = "_")
      new_values <- loadedFile$data[['pulse_transform']] * loadedFile$data[[input$yValColumnInput]]
      loadedFile$data[, (transformedColumn$name) := new_values]
      refilterSubset()
    })

    shiny::observeEvent(input$yValColumnInputButton, {
      if (is.null(loadedFile$data))
        return(NULL)
      # message(input$yValColumnInput)
      changeTransformedColumn()

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

    observe({
      if(!is.null(input$inputDirInput) && !is.null(input$outputDirInput)) {
        input_filepaths <- list.files(input$inputDirInput, include.dirs = FALSE)
        input_filepaths <- input_filepaths[!grepl("exe|png|jpg|jpeg|svg|pdf|tiff|bmp$", input_filepaths,ignore.case = TRUE)]
        input_filenames <- basename(input_filepaths)
        hasOutput <- input_filenames %in% list.files(input$outputDirInput, include.dirs = FALSE)

        shiny::updateSelectizeInput(session,
                                    inputId = "fileSelectBox",
                                    choices = paste0(input_filepaths, c("*", "")[hasOutput+1]))
      }
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
                       # message("Click")
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
                       # message("Click")
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
                         # updatePlotSettingsData()
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
                       inline_kbd_button('o', " - {KEY}: Open in Praat"),
                       inline_kbd_button('j', " - {KEY}: Play current audio file/selection"),
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

      shinyjs::removeClass("flagSamplesButton", class = "btn-warning")
      shinyjs::addClass(id = 'flagSamplesButton',class = "btn-success")
      shiny::updateActionButton(session, "flagSamplesButton", icon = icon("check"))
      shinyWidgets::updateMaterialSwitch(session, "useFlaggedColumnToggle", value = TRUE)
      set_selectize_choices(session, "colorCodeColumnInput", loadedFile, 'flagged_samples')()
      refilterSubset()

    })

    audioInfo <-
      praatServer("praatIO",
                  loadedFile,
                  fileHandler,
                  reactive(input$filenameColumnInput),
                  reactive(input$pitchRangeInput),
                  filenav$saveData,
                  reactive(input$navbar),
                  reactive(input$plot_brush))

    audioServer <- playAudioServer("playAudio",
                    loadedFile,
                    currentWave,
                    destroyLoadedAudio,
                    audioInfo,
                    nPlotted,
                    plotSubset,
                    reactive(input$plot_brush))

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


    filenav <- fileNavServer("fileNav",
                             loadedFile,
                             fileHandler,
                             reactive(input$saveOptionButton),
                             reactive(input$skipCheckedFilesToggle),
                             reactive(input$outputDirInput),
                             reactive(input$fileSelectBox),
                             reactive(input$filenameColumnInput),
                             nPlotted,
                             annotations,
                             refilterSubset,
                             destroyLoadedAudio,
                             loadFile$fileDelimiter,
                             plotFlag)

    octaveShift <- octaveShiftServer('octaveShift',
                                     loadedFile,
                                     plotSubset,
                                     transformedColumn,
                                     selectedPoints,
                                     lastTransformation,
                                     getBrushedPoints,
                                     updatePlot,
                                     reactive(input$yValColumnInput),
                                     reactive(input$pitchRangeInput),
                                     reactive(input$lockButton))

    diagnostics <- diagnosticsServer('diagnostics',
                                     loadedFile,
                                     parent_session = session,
                                     fileHandler,
                                     transformedColumn,
                                     reactive(input$xValColumnInput),
                                     reactive(input$yValColumnInput),
                                     reactive(input$filenameColumnInput),
                                     selectionColumn,
                                     refilterSubset,
                                     updatePlot)

  }

  shiny::shinyApp(ui, server)
}


