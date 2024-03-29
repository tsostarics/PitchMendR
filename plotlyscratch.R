library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("CSV File Plotter"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      selectInput("xcol", "X Variable", NULL),
      selectInput("ycol", "Y Variable", NULL),
      selectInput("files", "Files", NULL,multiple = TRUE,selectize = TRUE),
      actionButton("replotButton", "Replot"),
      actionButton("flipButton", "Flip Points"),
      actionButton("doubleButton", "Double")
    ),

    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("selectedPoints")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Function to read CSV file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    # print(colnames(df))
    return(df)
  })

  file_db <- reactiveValues(data = NULL)

  file_info <- reactiveValues(fnames = NULL,
                              is_plotted = NULL,
                              n = NULL)

  file_indices <- reactiveValues(fn = c())

  # Update variable options based on CSV input
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "xcol", choices = names(data()), selected = "t_ms")
      updateSelectInput(session, "ycol", choices = names(data()), selected = "f0")

      file_db$data <- split(data(), ~Filename)

      # Update File info
      file_info$fnames <- names(file_db$data)
      file_info$n <- length(file_info$fnames)
      file_info$is_plotted <- `names<-`(rep(FALSE,file_info$n), file_info$fnames)

      updateSelectInput(session, "files", choices = file_info$fnames, selected=NULL)
    }
  })

  observeEvent(input$doubleButton, {
    req(file_indices)
    selected_points <- event_data("plotly_selected")

    selected_curvepoints <- split(selected_points, ~curveNumber)
    curve_numbers <- names(selected_curvepoints)

    for (curve_i in curve_numbers) {
      filename <- file_indices$fn[as.integer(curve_i)]
      rows_to_modify <- selected_curvepoints[[curve_i]][["pointNumber"]]+1
      file_db$data[[filename]][[input$ycol]][rows_to_modify] <- 2*file_db$data[[filename]][[input$ycol]][rows_to_modify]
      plotly::plotlyProxy("plot", session) |>
        plotly::plotlyProxyInvoke("restyle", list(y = list(file_db$data[[filename]][[input$ycol]])), c(0,-1)+as.integer(curve_i))
    }
  })

  observeEvent(input$replotButton, {
    req(file_indices)
    # req(file_db$file)

    selected_files <- input$files
    plotted_files <- file_info$fnames[file_info$is_plotted]
    files_to_add <- selected_files[!selected_files %in% plotted_files]
    files_to_remove <- plotted_files[!plotted_files %in% selected_files]


    # If there are any files to remove from the plot, presupposing they have
    # already been plotted before, hide the traces
    if (length(files_to_remove) != 0) {
    print(paste0("To remove: ", paste0(files_to_remove, collapse = ", ")))

    remove_indices <- which(file_indices$fn %in% files_to_remove) # The traces are 0 indexed but 0 is a placeholder trace, so we can treat the data traces as 1 indexed
    plotly::plotlyProxy("plot", session) |>
      plotly::plotlyProxyInvoke("restyle", list(visible = FALSE), remove_indices)

    file_info$is_plotted[unique(files_to_remove)] <- FALSE
    }

    # After we've removed traces, we can now add new traces if there are any
    if (length(files_to_add) != 0) {
      print(paste0("To add:",  paste0(files_to_add, collapse = ", ")))

      to_be_traced <- !files_to_add %in% file_indices$fn

      add_new_file_trace <- function(filenames) {
        for (f in filenames) {

        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = file_db$data[[f]][[input$xcol]],
              y = file_db$data[[f]][[input$ycol]],
              line = list(color = 'red'),
              name = f
            )
          ) |>
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              data = file_db$data[[f]],
              x = ~get(input$xcol),
              y = ~get(input$ycol),
              # x = file_db$data[[f]][[input$xcol]],
              # y = file_db$data[[f]][[input$ycol]],
              type = "scatter",
              mode = "markers",
              marker = list(
                color = ~get('flagged_samples')
                # color =  ~file_db$data[[f]][['flagged_samples']]
                ),
              name = f
            )
          )
        }
        file_indices$fn <<- c(file_indices$fn, rep(filenames, each=2)) # These are in order already, *2 for the markers

      }

      for (f in files_to_add[to_be_traced]){
        # Add traces if they don't exist yet. Setting visible to FALSE here
        # allows us to change a large number of traces to visible later
        # so that they appear all at once instead of one at a time.
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "addTraces",
            # note this acts like a manually created dataframe
            list(
              x =  file_db$data[[f]][[input$xcol]],
              y =  file_db$data[[f]][[input$ycol]],
              line = list(color = 'red'),
              name = f,
              visible = FALSE
            )
          ) |>
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x =  file_db$data[[f]][[input$xcol]],
              y =  file_db$data[[f]][[input$ycol]],
              type = "scatter",
              mode = "markers",
              marker = list(color =  ifelse(file_db$data[[f]][['flagged_samples']], "blue", "magenta"),
                            symbol = ifelse(file_db$data[[f]][['flagged_samples']], 1, 2)),
              name = f,
              visible = FALSE
            )
          )

        file_indices$fn <- c(file_indices$fn, rep(files_to_add[to_be_traced], each=2)) # These are in order already, *2 for the markers
      }

      # Turn the visibility on for all the traces to be shown
      plotly::plotlyProxy("plot", session) |>
        plotly::plotlyProxyInvoke("restyle", list(visible = TRUE), which(file_indices$fn %in% files_to_add))

      file_info$is_plotted[unique(files_to_add)] <- TRUE
    }


  })

  # Prerender the base plot to add traces to
  output$plot <- renderPlotly({
    req(input$xcol, input$ycol)
    plotly::plot_ly() |>
      plotly::layout(dragmode = "select",
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)") |>
      plotly::event_register("plotly_selected") |>
      plotly::event_register("plotly_click")
  })

  output$selectedPoints <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
  })

  observeEvent(input$flipButton, {
    print('click')
    d <- event_data("plotly_selected")
    if (is.null(d))
      return(NULL)

    selected_curvepoints <- split(d, ~curveNumber)
    curve_numbers <- names(selected_curvepoints)

    for (curve_i in curve_numbers) {
      filename <- file_indices$fn[as.integer(curve_i)]
      rows_to_modify <- selected_curvepoints[[curve_i]][["pointNumber"]]+1
      file_db$data[[filename]][['flagged_samples']][rows_to_modify] <- !file_db$data[[filename]][['flagged_samples']][rows_to_modify]
      plotly::plotlyProxy("plot", session) |>
        plotly::plotlyProxyInvoke("restyle",
                                  list(marker = list(color = ifelse(file_db$data[[filename]][['flagged_samples']],
                                                                    "blue",
                                                                    "magenta")), as.integer(curve_i)))
    }

  },ignoreInit = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
