howto_UI <- function(id) {
  ns <- NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(""),
    bslib::card_body(
      shiny::markdown(c(
        "## Quick Start",
        "Load a file in the Setup tab by entering the desired directory and loading a CSV file containing your pitch contours.",
        "In the web version, simply upload a csv file from your computer to the app.",
        "Individual files in the loaded dataset will be visualized in the Editor tab.",
        "Select points to remove, halve, or double, then save the annotated dataset with the `[Save File]` button.",
        "Load the `demo_data.csv` file by pressing the `[Load File]` button on the Setup tab with the default settings to use an example dataset."
      )),

      shiny::markdown(c(
        "## Working in the Editor",
        "The editor page is arranged with the layout below. The Tools sidebar is able to be hidden when not needed.")),
      tags$img(src = "img/layout.svg", width = "50%", style = "max-height:300px"),
      shiny::markdown(c(
        "Upon loading a file, the contours in the dataset will all be plotted.",
        "You can click on individual pulses to remove them, or select a region of pulses by clicking and dragging (or 'brushing').",
        "The plot will automatically resize whenever the window size changes, and you can adjust the size of the plot by clicking and dragging the bottom-right corner."
      )),
      tags$img(src = "img/plot_manyfiles.svg", width = "50%", style = "max-height:300px"),
      shiny::markdown(c(
        "You can 'zoom in' on the contours that contain the selected pulses by pressing the `[Plot Brushed Files]` button in the plot settings panel.",
        "The selected pulses can be transformed using the buttons in the Pulse Transformation panel."
      )),
      tags$img(src = "img/transform_buttons.svg", width = "50%", style = "max-height:300px"),
      shiny::markdown(c(
        "",
        " - `[Toggle Pulses]` will flip whether the selected pulses are removed or kept.",
        " - `[Remove]` will remove selected pulses from the contour, but they will still be visible as triangles instead of circles.",
        " - `[Keep]` will keep selected pulses, in case you change your mind about removing certain pulses.",
        " - `[Halve Pulses]` will divide the y-value (usually Frequency in Hz) by 2 to correct for doubling errors.",
        " - `[Double Pulses]` will multiply the y-value by 2 to correct for halving errors.",
        "",
        "Pulses that are removed (shown as triangles) will not be included when a line is drawn through the pulses.",
        "Additionally, pulses that have been doubled or halved will be transformed in the plot in real time.",
        "If you want to compare the transformed contours to the original contours, you can use the toggles in the Tools sidebar.",
        "If you want to undo the last halving/doubling transformation you made, you can press the `[Undo Transform]` button."
      )),
      tags$img(src = "img/sidebar_toggles.svg", width = "25%", style = "max-height:300px"),
      shiny::markdown(c(
        "## File Navigation and Annotations",
        "The file navigation and annotation panel provides tools to quickly flip between individual files.",
        "",
        " - `[ < ]` (Previous) will move to the previous file alphabetically.",
        " - `[ > ]` (Next) will move to the next file alphabetically.",
        " - `[Save File]` will save the current file to the output file directory specified in the Settings tab.",
        " - A row of badges can be used to quickly tag a file with consistent annotations (customizability for these will be released in a future update)",
        " - A note pad is available to make quick notes.",
        "",
        "When navigating from an individual file to another file, the app will automatically assume that the file you're currently looking at has been 'checked.'",
        "Keeping track of which files you've already checked helps to know when you've finished annotating the dataset.",
        "By default, file navigation using the Previous/Next buttons will automatically skip files you've already checked.",
        "You can navigate to specific files that have been checked or unchecked using the dropdown menus in the sidebar.",
        "You can also check off all files currently shown in the plot by using the `[Check off visible]` button in the sidebar."
      )),
      tags$img(src = "img/checked_dropdowns.svg", width = "50%", style = "max-height:300px"),
      shiny::markdown(c(
        "The following options are also available in the UI Options portion of the Settings tab",
        " - The row of badges and the notepad can be hidden if not needed.",
        " - The output file can be saved every time you navigate to a new file. Note that enabling this will increase latency when rerendering the plot.",
        " - Disable skipping over files that have already been checked when using the Previous/Next buttons",
        " - Whether to use keyboard shortcuts for various buttons. See the (?) icon by this option in the settings pane for more information.",
        "",
        "### Navigating with regular expressions",
        "You can use regular expressions (regexes) to selectively plot different groups of contours.",
        "If you are unfamiliar with regexes, you can consult this chapter from R for Data Science: [https://r4ds.hadley.nz/regexps#introduction](https://r4ds.hadley.nz/regexps#introduction)",
        "The most common use for navigating with regular expressions is to go from plotting 1 contour to plotting all contours in the dataset.",
        "For this you can write `.` (a single period, the default) and press the `[Plot Matches]` button.",
        "`.` will match any character, hence matching with all files in the dataset.",
        "",
        "Using regexes can be a useful way to quickly show subsets of the data; ",
        "for example, the demo data contains contours under three broad classes: rises (`lhh`), falls (`hll`), and rise-fall-rise (`rfr`).",
        "The entire rising subset can be plotted by typing the regex `lhh` and pressing the [Plot Matches] button.",
        "Similarly, all contours except the falling contours could be plotted by using the regex `lhh|rfr`.",
        "Note that how useful the regex functionality is will depend on the metadata encoded in your dataset's filenames.",
        "",
        "## Praat and Audio Integration",
        "The following tools are exclusive to the local version of PitchMendR, and are not available in the web version.",
        "You can interface directly with Praat by setting the relevant directories in the Audio File Options portion of the Setup tab.",
        "You can then open the files that are currently displayed in the main plot by clicking the `[Open in Praat]` button in the sidebar.",
        "This can be useful when it's not clear how to handle an error based on the extracted pitch contour, and so you can double check the spectrogram quickly."
      )),
      tags$p("You can also press the ",
             tags$span(icon("file-circle-minus")),
             " icon to remove all the currently open objects in the Praat object pane. For more information, please see the (?) icon in the Audio File Options portion of the setup tab."),
      shiny::markdown(c(
        "",
        "If a single file is currently displayed in the plot, then you can also press the `[Play File]` button in the Tools sidebar to play the file through the app without opening it in Praat."
      ))
    )
  )
}

# howtoServer <- function(id) {
#   # Not used
#   moduleServer(id, function(input, output, session) {} ) }
