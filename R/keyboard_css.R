#' Use keyboard-css from online build
#'
#' If you don't want to use the local version for some reason, you can use
#' this function to load the keyboard-css stylesheet from the online build.
#' You must place either this or `use_keyboardcss` somewhere in your UI.
#'
#' @return A singleton tagList for the UI
#' @importFrom shiny singleton HTML
use_keyboardcss_link <- function() {
  shiny::singleton(tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/keyboard-css@1.2.4/dist/css/main.min.css")
    )
  )
  )
}

#' Use keyboard-css from local installation
#'
#' You must place this somewhere in your UI to use keyboard-css.
#' This will load the keyboard-css stylesheet from the one saved in the package.
#'
#' @return A singleton tagList for the UI
#'
use_keyboardcss <- function() {
  shiny::singleton(
    htmltools::includeCSS(
      system.file("keyboardcss/keyboardcss.css", package = "PitchMendR")
    )
  )

}

#' Insert a kbd button
#'
#' This function creates a kbd tag using keyboard-css.
#' This type of button will not animate when clicked.
#'
#' @param key String denoting a key to use, e.g., 'k' or 'ctrl'. If passed a
#' character vector, only the first element is used.
#' @param class Additional classes to set on the button. Default NULL will
#' set class kbc-button.
#' @param size Size to apply to the button. Default "sm" will set class
#' kbc-button-sm. See https://github.com/shhdharmen/keyboard-css for more info
#'
#' @return A kbd tag with the appropriate classes and key
kbd <- function(key, class=NULL, size = "sm") {
  if(!is.null(size))
    size <- paste0("kbc-button-", size)

  class <- c(class, size)
  key <- toupper(key)

  tags$kbd(class = c(class, size), key)
}

#' Insert a kbd button
#'
#' This function creates a button tag using keyboard-css.
#' This type of button will animate when clicked.
#' You can pass an id to add event listeners elsewhere in a server.
#'
#' @param key String denoting a key to use, e.g., 'k' or 'ctrl'. If passed a
#' character vector, only the first element is used.
#' @param class Additional classes to set on the button. Default NULL will
#' set class kbc-button.
#' @param size Size to apply to the button. Default "sm" will set class
#' kbc-button-sm. See https://github.com/shhdharmen/keyboard-css for more info
#' @param id An id to set on the button. This is useful for adding event
#' listeners or other JavaScript functionality.
#'
#' @return A button tag with the appropriate classes and key
kbd_button <- function(key, class=NULL, size = "sm", id = NULL) {
  stopifnot(is.character(key))
  key <- key[1]
  if (is.null(class))
    class <- 'kbc-button'

  if(!is.null(size))
    size <- paste0("kbc-button-", size)

  class <- c(class, size)
  key <- toupper(key)

  tagList(tags$button(id = id, class = class, style = "width:fit-content", key))

}

#' Interpolate string with inline kbd button
#'
#' Inserts inline kbd buttons
#'
#' @param key Character vector of keys to display
#' @param string A glue string to interpolate. Use `{KEY}` to insert the inline
#' key if you're only using one key, or `{KEY[1]}`, `{KEY[2]}`, etc. if you're
#' inserting multiple keys. Note that you are only allowed to interpolate
#' `KEY`, other R code will not be evaluated.
#' @param ... Additional arguments passed to `kbd_button()`
#'
#' @return An interpolated string with inline kbd buttons
#'
#' @examples
#' \dontrun{
#'
#' inline_kbd_button("a", "Press {KEY} to continue")
#' # > Press <button class="kbc-button kbc-button-sm" style="width:fit-content">A</button> to continue
#'
#' inline_kbd_button(c("ctrl", "z"), "Press {KEY[1]}+{KEY[2]} to undo")
#' # > Press
#' <button class="kbc-button kbc-button-sm" style="width:fit-content">CTRL</button>
#' +
#' <button class="kbc-button kbc-button-sm" style="width:fit-content">Z</button>
#' to undo
#' }
inline_kbd_button <- function(key, string, ...) {
  KEY <- lapply(key, \(k) kbd_button(k, ...))
  if (length(KEY) == 1)
    KEY <- KEY[[1]]

  # Create new environment to allow user to set multiple keys using
  # KEY[1], KEY[2], etc. without allowing them to execute other R code
  eval_environment <- rlang::new_environment(data = list("KEY" = KEY, '[' = `[[`))
  glue::glue(string,.envir = eval_environment)
}


#' Interpolate string with inline kbd button
#'
#' Inserts inline kbd tags
#'
#' @param key Character vector of keys to display
#' @param string A glue string to interpolate. Use `{KEY}` to insert the inline
#' key if you're only using one key, or `{KEY[1]}`, `{KEY[2]}`, etc. if you're
#' inserting multiple keys. Note that you are only allowed to interpolate
#' `KEY`, other R code will not be evaluated.
#' @param ... Additional arguments passed to `kbd()`
#'
#' @return An interpolated string with inline kbd buttons
#'
#' @examples
#' \dontrun{
#'
#' inline_kbd_button("a", "Press {KEY} to continue")
#' # > Press <button class="kbc-button kbc-button-sm" style="width:fit-content">A</button> to continue
#'
#' inline_kbd_button(c("ctrl", "z"), "Press {KEY[1]}+{KEY[2]} to undo")
#' # > Press <button class="kbc-button kbc-button-sm" style="width:fit-content">CTRL</button>+<button class="kbc-button kbc-button-sm" style="width:fit-content">Z</button> to undo
#' }
inline_kbd <- function(key, string, ...) {
  KEY <- lapply(key, \(k) kbd(k, ...))
  if (length(KEY) == 1)
    KEY <- KEY[[1]]

  # Create new environment to allow user to set multiple keys using
  # KEY[1], KEY[2], etc. without allowing them to execute other R code
  eval_environment <- rlang::new_environment(data = list("KEY" = KEY, '[' = `[[`))
  glue::glue(string,.envir = eval_environment)
}

# make_key_interactive <- function(key) {
#   paste0(
#     "
#                  document.addEventListener('keydown', (ev) => {
#                     const key = ", key, ";
#                     const element = document.querySelector(
#                         '[data-keyboard-key=\"' + key.toUpperCase() + '\"]'
#                     );
#                     element.classList.add('active');
#                 });
#
#                 document.addEventListener('keyup', (ev) => {
#                     const key = ev.key;
#                     const element = document.querySelector(
#                         '[data-keyboard-key=\"' + key.toUpperCase() + '\"]'
#                     );
#                     element.classList.remove('active');
#                 });
#
#                  "
#   )
# }
