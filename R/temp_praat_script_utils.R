#' Run a praat script that deletes itself
#'
#' Praat streams in the script lines instead of loading the whole file into
#' memory, so we need to wait until the script is done running before we try and
#' delete the file. BUT, if we use wait = TRUE, the script won't send back a
#' return value even if you use exitScript() as the last line. To get around
#' this, we have the praat script delete itself when it's done executing (hence
#' the deleteFile line) and then wait until the file stops existing so we can
#' continue.
#'
#' @param lines Character vector, each string is a line of the praat script
#' @param praat_path Path to praat, use audioInfo$praatPath() outside of the
#' praat module
#'
#' @return Invisibly returns 0
run_temp_script <- function(lines, praat_path = "./Praat.exe") {
  temp_script <- tempfile(tmpdir = getwd(), fileext = ".praat")

  writeLines(c(lines, paste0('deleteFile: "', temp_script, '"')),
             temp_script)

  systemcall <- paste0(praat_path,
                       ' --send --hide-picture "',
                       temp_script,
                       '"',
                       sep = " ")

  message("Praat script start: ", Sys.time())
  message("If praat throws an error, close praat and delete ", temp_script, " to return")
  message(systemcall)
  base::system(systemcall, wait = FALSE) # Runs the script, deletes self at end
  while (file.exists(temp_script)) {}    # Waits until the script has deleted itself
  message("Praat script end: ", Sys.time()) # Continue on

  invisible(0)
}
