#' Update selectize choices once file is loaded
#'
#' The selectizeInputs used for the column names use sensible default values
#' based on the output of praatsauce. However, once a file is loaded, the
#' selectizeInput should be updated to reflect the column names of the loaded
#' file.
#'
#' @param session Shiny session
#' @param inputId ID of the selectizeInput to work with
#' @param data_ref Reference to the loaded data
#' @param input_value Current value of the selectizeInput, a column name
#' @param add Whether to add the current value to the data as a new column
#' if it doesn't already exist
#' @param add_with If we're adding a column (add=TRUE), then use this value to
#' fill the column with. Defaults to TRUE but can be any singular value.
#'
#' @return A reactive function that updates the selectizeInput, should be called
#' immediately
#'
#' @examples
#' \dontrun{
#'
#' ui <- fluidPage(
#'  # A select input with choices "a" and default value "a" (input$mySelectInput == "a")
#'  selectizeInput("mySelectInput", label="Select here", choices = c("a"), selected = "a"),
#'  actionButtion("myButton", "Click me")
#' )
#'
#' server <- function(input, output, session) {
#'  data_ref <- reactiveValues(data = NULL)
#'  data_ref$data <- data.frame(b = c(1,2,3), c = c(4, 5, 6))
#'
#'  observeEvent("myButton", {
#'    # Update the selectizeInput to reflect the column names of the loaded data
#'    set_selectize_choices(session, "mySelectInput", data_ref, input$mySelectInput)()
#'
#'    # Now the selectizeInput has choices "b" and "c" and the selected value is "b" (the first value)
#'  })
#'
#' }
#'
#' }
#'
#'
set_selectize_choices <- function(session, inputId, data_ref, input_value, add = FALSE, add_with = TRUE) {
  reactive({
    if (is.null(data_ref$data))
      return(NULL)

    if (add) {
      if (!input_value %in% colnames(data_ref$data)) {
        if (length(add_with) > 1)
          warning("add_with is a vector of length > 1, only using first element")

        data_ref$data[, (input_value) := add_with]
                      }
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
