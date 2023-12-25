clean_file <- function(file_selection) {
  gsub("[*]$", "", file_selection)
}
