#' Create a key bind action with optional message
#'
#' Bundles together a message with a reactive expression to be used as a key
#' binding action.
#'
#' @param bindexpr Reactive expression or function to execute
#' @param msg Message to use, no message is sent if NULL
#'
#' @return A function that sends a message and executes the given function or
#' reactive
keyBindAction <- function(bindexpr, msg = NULL) {
  function(){
    if (!is.null(msg))
      message(msg)

    bindexpr()
  }
}

#' Change key binding
#'
#' In this app we're storing the expressions to execute for each key in an
#' updateable reactive values object. By doing so we can modify key bindings
#' more easily later. This function adds a key to the currently tracked keys
#' and sets the action to execute when the key is pressed.
#'
#' @param key Key to bind
#' @param bindexpr Reactive expression to bind to the key
#' @param msg Optional message to use
#'
#' @return Nothing, changes boundKeys
# changeKeyBinding <- function(key, bindexpr, msg = NULL) {
#   if (is.null(boundKeys) | !is.character(key))
#     return(NULL)
#   keys::addKeys("keys", key, session)
#   boundKeys$keys[key] <<- keyBindAction(msg, bindexpr)
# }

#' Remove key binding
#'
#' Removes a tracked key and removes its reactive expression from the
#' reactive values object.
#'
#' @param key Key to remove
#'
#' @return Nothing, changes boundKeys
# removeKeyBinding <- function(key) {
#   if (is.null(boundKeys) | !is.character(key))
#     return(NULL)
#   keys::removeKeys("keys", key, session)
#   boundKeys$keys[key] <<- NULL
# }
