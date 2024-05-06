playAudioUI <- function(id) {
  ns <- NS(id)

  tagList(
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
    span(style = "display:inline-block",
         # HTML(
    shiny::actionButton(
      inputId = ns("playVisibleFile"),
      label = "Play File",
      title = "Click to play file (if one is plotted & available)",
      icon = shiny::icon("xmark"),
      class = "bigPlayButton"
    ),
    shiny::actionButton(
      inputId = ns("stopButton"),
      label = NULL,
      title = "Click to stop playing file",
      icon = shiny::icon("stop", style = "font-size:1.5em;"),
      class = "hideStopButton"
    )
    )
  )
}

playAudioServer <- function(id, loadedFile, currentWave, destroyLoadedAudio,
                            audioInfo, nPlotted, plotSubset, plot_brush) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    observe({
      if (is.null(loadedFile$data) ||
          !nPlotted$is_one ||
          is.null(audioInfo$audioDirectory) ||
          is.null(audioInfo$glueString) ||
          (!is.null(currentWave$exists) && !currentWave$exists)) {
        shiny::updateActionButton(session, "playVisibleFile",icon = icon("xmark"))
      } else if (nPlotted$is_one && (is.null(currentWave$value))) {
        shiny::updateActionButton(session, "playVisibleFile",icon = icon("file-arrow-up"))
      } else if (nPlotted$is_one) {
        shiny::updateActionButton(session, "playVisibleFile",icon = icon("play"))
      } else {
        shiny::updateActionButton(session, "playVisibleFile",icon = icon("triangle-exclamation"))
      }
    })

    get_playback_region <- shiny::reactive({
      req(currentWave)
      req(plot_brush)
      # If no region is selected, play the whole file
      if (is.null(plot_brush()))
        return(seq.int(0, length(currentWave$value)))


      # TODO: Add these values to be automatically computed whenever a new file is loaded
      rate <- attr(currentWave$value, "rate")
      fileLength <- length(currentWave$value) / rate

      # Get the bounds of the region, ensure there are no negative values
      t <- c(max(c(plot_brush()$xmin,0)), max(c(plot_brush()$xmax, 0)))

      # If the selected region is beyond the length of the file or somehow
      # only a single sample is selected, just play the whole file
      if (t[1L] > fileLength || t[1L] == t[2L])
        return(seq.int(0, length(currentWave$value)))

      seq.int(t[1L]*rate, t[2L]*rate)

    })

    shiny::observeEvent(input$stopButton, {
      if (is.null(currentWave$instance))
        return(NULL)

      audio::pause.audioInstance(currentWave$instance)
      audio::rewind.audioInstance(currentWave$instance)
    })

    shiny::observeEvent(input$playVisibleFile, {
      # This button is only active if we have data loaded, we've set the audio
      # directory information, and we're only looking at one file
      if (is.null(loadedFile$data) || !nPlotted$is_one || is.null(audioInfo$audioDirectory) || is.null(audioInfo$glueString))
        return(NULL)

      # Create the audio path for the currently opened file using the
      # glue string and audio directory that the user specified
      #loadedFile$data[loadedFile$data[[input$filenameColumnInput]] %in% fileHandler$filenames[fileHandler$isPlotted],],
      file_to_open <-
        file.path(audioInfo$audioDirectory(),
                  glue::glue_data_safe(plotSubset$data, audioInfo$glueString())[1])


      if (!file.exists(file_to_open)) {
        message(paste0("File ", file_to_open, " not found."))
        shiny::updateActionButton(session, "playVisibleFile",icon = icon("xmark"))
        currentWave$exists <- FALSE

        return(NULL)
      }

      currentWave$exists <- TRUE

      shinyjs::addClass(("playVisibleFile"), "smallPlayButton")
      shinyjs::removeClass("playVisibleFile", "bigPlayButton")
      shinyjs::addClass(("stopButton"), "showStopButton")
      shinyjs::removeClass(("stopButton"), "hideStopButton")
      # If the file exists, then we need to load it as a wave object.
      # Once it's loaded, then it can be played, but we need to make sure to
      # close any running audio instances, otherwise (at best) the audio
      # will be overlaid or (at worst) the app will crash.
      if (is.null(currentWave$value)){
        currentWave$path <- file_to_open
        currentWave$value <- audio::load.wave(file_to_open)
        if (!is.null(currentWave$instance))
          close(currentWave$instance)
        currentWave$instance <- audio::play(currentWave$value[get_playback_region()])
      } else {
        if (!is.null(currentWave$instance))
          close(currentWave$instance)
        currentWave$instance <- audio::play(currentWave$value[get_playback_region()])
      }
    })
  })
}
