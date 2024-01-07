columnInfo_UI <- function(id) {
  bslib::card(
    class = "h-100",
    title = "Column Info",
    shiny::markdown(
      mds = c(
        "## PitchMendR Columns",
        "",
        "Upon opening a new dataframe to annotate, the following columns are added if they do not exist. `{Y}` depends on what you set as the Y-axis variable.",
        " - `keep_pulses`: Logical, `TRUE` if the point should be retained, `FALSE` if it should be removed. Use this to filter out pulses in your analysis.",
        " - `pulse_transformation`: Numeric, multiply this by the original Hz values to obtain the transformed values corrected for halving/doubling errors. Values are typically 1 (no change), 2 (corrected for halving), or 0.5 (corrected for doubling).",
        " - `{Y}_transformed`: Numeric, the values of `{Y}` multiplied by `pulse_tansformation`.",
        " - `file_checked`: Logical, `TRUE` if the file has been checked already, otherwise `FALSE`. Used to keep track of your progress.",
        " - `tags`: Character, a `+` delimited string of which tags have been applied using the badges on the editor pane. E.g., tagging a file with *Unusable* and *Good Example* will be represented as `Unusable+Good Example`.",
        " - `notes`: Character, the annotaion entered in the notepad for a file.",
        " - `pulse_id`: Integer index of each pulse, i.e. the row number, used as a unique ID.",
        "",
        "The following columns are also added if the `[Flag Samples]` button is pressed.",
        " - `F0_semitones`: {Y} converted to semitones from the median value across all files. Used to find octave jumps.",
        " - `flagged_samples`: Logical, `TRUE` if the F0 sample has been flagged as a potential error, `FALSE` otherwise.")
    ),
  )
}
