#' Use keyboard-css from online build
#'
#' If you don't want to use the local version for some reason, you can use
#' this function to load the keyboard-css stylesheet from the online build.
#' You must place either this or `use_keyboardcss` somewhere in your UI.
#'
#' @return A singleton for the UI
#' @importFrom shiny singleton HTML
use_keyboardcss_link <- function() {
  shiny::singleton(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://unpkg.com/keyboard-css@1.2.4/dist/css/main.min.css")
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

  tags$button(id = id, class = class, style = "width:fit-content", key)

}

#' Interpolate string with inline kbd button
#'
#' Inserts inline kbd buttons via string interpolation.
#'
#' @param key Character vector of keys to display
#' @param string A glue string to interpolate. Use `{KEY}` to insert the inline
#'   key or a chord of keys if `use_chord` is TRUE. Use `{KEY[1]}`, `{KEY[2]}`,
#'   etc. if you're inserting multiple keys and `use_chord` is FALSE. Note that
#'   you are only allowed to interpolate `KEY`, other R code will not be
#'   evaluated.
#' @param ... Additional arguments passed to `kbd_button()`
#' @param as_chord Whether to treat `key` as a chord with multiple keys
#'   involved. If TRUE, will automatically expand `{KEY}` to
#'   `{KEY[1]}+{KEY[2]}+...`. If FALSE, make sure to use `{KEY[1]}`, `{KEY[2]}`,
#'   etc. or you'll get a list of buttons instead of a string. When `keys` is a
#'   character vector of length 1, this argument doesn't affect anything.
#' @param safe If TRUE, will throw an error if the resulting interpolated string
#'   is not of length 1. Usually this happens when as_chord is FALSE and you
#'   only provide `{KEY}` instead of `{KEY[1]}`.
#'
#' @return An interpolated string with inline kbd buttons
#'
#' @examples
#' \dontrun{
#'
#' inline_kbd_button("a", "Press {KEY} to continue") #== "Press a to continue"
#' inline_kbd_button(c("ctrl", "z"), "Press {KEY} to undo") #== "Press ctrl+z to undo"
#' inline_kbd_button(c("ctrl", "z"), "Press {KEY[1]} and {KEY[2]} to undo", as_chord=FALSE)
#' }
inline_kbd_button <- function(key, string, as_chord=TRUE, safe=TRUE, ...) {
  .make_inline_kbd(key, string, as_chord, safe, kbd_button, ...)

}


#' Interpolate string with inline kbd button
#'
#' Inserts inline kbd tags
#'
#' @param key Character vector of keys to display
#' @param string A glue string to interpolate. Use `{KEY}` to insert the inline
#'   key or a chord of keys if `use_chord` is TRUE. Use `{KEY[1]}`, `{KEY[2]}`,
#'   etc. if you're inserting multiple keys and `use_chord` is FALSE. Note that
#'   you are only allowed to interpolate `KEY`, other R code will not be
#'   evaluated.
#' @param ... Additional arguments passed to `kbd()`
#' @param as_chord Whether to treat `key` as a chord with multiple keys
#'   involved. If TRUE, will automatically expand `{KEY}` to
#'   `{KEY[1]}+{KEY[2]}+...`. If FALSE, make sure to use `{KEY[1]}`, `{KEY[2]}`,
#'   etc. or you'll get a list of buttons instead of a string. When `keys` is a
#'   character vector of length 1, this argument doesn't affect anything.
#' @param safe If TRUE, will throw an error if the resulting interpolated string
#'   is not of length 1. Usually this happens when as_chord is FALSE and you
#'   only provide `{KEY}` instead of `{KEY[1]}`.
#'
#' @return An interpolated string with inline kbd buttons
#'
#' @examples
#' \dontrun{
#'
#' inline_kbd("a", "Press {KEY} to continue")#'
#' inline_kbd(c("ctrl", "z"), "Press {KEY[1]}+{KEY[2]} to undo")
#' }
inline_kbd <- function(key, string, as_chord=TRUE, safe=TRUE, ...) {
  .make_inline_kbd(key, string, as_chord, safe, kbd, ...)
}


.make_inline_kbd <- function(key, string, as_chord=TRUE, safe=TRUE, .kbd_method, ...) {
  KEY <- lapply(key, \(k) .kbd_method(k, ...))

  if (as_chord)
    KEY <- paste0(vapply(KEY, as.character, "char"), collapse = "+")

  # Create new environment to allow user to set multiple keys using
  # KEY[1], KEY[2], etc. without allowing them to execute other R code
  eval_environment <- rlang::new_environment(data = list("KEY" = KEY, '[' = `[[`))
  interpolated_string <- glue::glue(string,.envir = eval_environment)

  if (length(interpolated_string) > 1 && safe)
    stop("More than 1 interpolated string was returned. Use as_chord=TRUE with {KEY} or use as_chord=FALSE and subset KEY with {KEY[1]}.")
  interpolated_string
}
