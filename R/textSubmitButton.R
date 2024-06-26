#' Text input with submit button
#'
#' Modified from the source for fileInput: https://github.com/rstudio/shiny/blob/main/R/input-file.R
#'
#' Create a text input with a submit button to the left of the text box.
#' The button can be styled with a font-awesome icon.
#' The text value's inputId is the inputId field, the button's inputId has
#' `Button` appended to it. E.g., inputId = myTextInput yields input$myTextInput
#' and input$myTextInputButton
#'
#'
#' @family input elements
#'
#' @param value Initial value.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @param icon The icon used for the button. Default share-from-square
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param disabled If `TRUE`, the button will not be clickable. Use
#'   [updateActionButton()] to dynamically enable/disable the button.
#' @param ... Named attributes to be applied to the button or link.
#'   option.
#' @export
#'
#' @importFrom htmltools div css span
submitTextInput <- function(inputId, label, value = "",
                      width = NULL, icon = "share-from-square",
                      placeholder = "Enter text here", disabled = FALSE, ...) {
  # Borrow unexported functions from shiny package and avoid notes
  validateIcon <- utils::getFromNamespace("validateIcon", "shiny")
  shinyInputLabel <- utils::getFromNamespace("shinyInputLabel", "shiny")

  value <- shiny::restoreInput(id = inputId, default = value)

  inputTag <- tags$button(id=paste0(inputId, "Button"),
                          style = htmltools::css(width = shiny::validateCssUnit(width),
                                                 `padding-left` = "10px",
                                                 `padding-right` = "10px",
                                                 border = "none"),
                          type="button",
                          class="btn btn-default action-button",
                          `data-val` = value,
                          disabled = if (isTRUE(disabled)) NA else NULL,
                          list(validateIcon(shiny::icon(icon))),
                          ...
  )

  div(class = "form-group shiny-input-container",
      style = htmltools::css(width = shiny::validateCssUnit(width)),
      shinyInputLabel(inputId, label),

      div(class = "input-group",
          # input-group-prepend is for bootstrap 4 compat
          tags$label(class = "input-group-prepend",
                     span(# shiny::icon(buttonLabel),
                          inputTag
                     )
          ),
          tags$input(id = inputId,
                     type="text",
                     class="shiny-input-text form-control",
                     value=value,
                     placeholder = placeholder)
      )
  )
}
