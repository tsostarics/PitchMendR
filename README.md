# PitchMendR

This package provides a shiny app used to modify F0 pitch pulses held in a spreadsheet.

## Citation

If you use this software in your research, please cite the following paper:

Sostarics, T., & Cole, J. (2024). *PitchMendR: A Tool for the Diagnosis and Treatment of F0 Irregularities*. In Proceedings of Speech Prosody 2024.

Bibtex:

```
@inproceedings{sostarics2024sp,
author = {Sostarics, Thomas and Cole, Jennifer},
year = {2024},
month = {6},
title = {{PitchMendR}: A Tool for the Diagnosis and Treatment of {F0} Irregularities},
booktitle = {Proceedings of Speech Prosody 2024},
}
```

## Installation

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("tsostarics/PitchMendR")
```

## Usage

You can launch the editor GUI like so:

```r
library(PitchMendR)
openEditor()
```

### Setup page

You will be brought to the `Setup` page, where the current working directory is shown.
The input directory will be automatically set to the directory containing a demo file `demo_data.csv`.
The output directory is set to the current working directory (`.`)
Files in the input directory that you can load are shown in the Files Available dropdown box.
Once you select a file from the box, press the big blue `Load File` button in the top right corner.
The app assumes the following columns exist by default, but can be changed in the Settings pane.

 - A `Filename` column specifying the individual pitch contours
 - A `t_ms` column to use as the x-axis timestamp values for the pulses
 - A `f0` column to use as the y-axis values

These are the default outputs from PraatSauce, which exports a single csv file containing information about multiple processed files.
If any of these columns are not found in the dataset, you'll be brought to the `Settings` page where you can change the column name mappings.
Once you've set the column names correctly you can try loading the file again.

### Editor pane

There are three main sections, from right to left:

 - The editor pane, where the top part plots pitch contours and the bottom part gives you buttons to modify the pitch pulses
 - A plot settings pane, where you can control what's plotted
 - A sidebar with various buttons, I'll skip these for now

#### Editor pane buttons

 - `[Show line]`: Toggles whether a line is drawn through retained pulses in the plot
 - `[<]` and `[>]`: Navigate to the previous/next file
 - `[Save file]`: Saves file to the output directory. If it exists, it will be overwritten.
 - `[Toggle Pulses]`: Select points in the plot area, then toggle whether they should be removed or kept
 - `[Keep]`: Keep selected pulses
 - `[Remove]`: Remove selected pulses
 - `[Halve Pulses]`: Divide pulse Hz values by 2, correcting for doubling errors
 - `[Double pulses]`: Multiply pulse Hz values by 2, correcting for halving errors

### Plot Settings pane inputs/buttons

 - Pitch range setting: Modify the minimum/maximum F0 values to plot. Will automatically compute a reasonable range based on the loaded file to start with.
 - Horizontal Zoom buttons: Based on the same buttons in Praat, allows for zooming in/out of specific regions.
 - Regex text input: Type a regular expression and press `[Plot matches]` to plot only those files whose filenames match the pattern. Set to `.` to show all files.
 - Number of files plotted: reflects number of files plotted
 - Brushed files: When you select pulses, the files those pulses belong to will be displayed. You can click the `[Plot Brushed Files]` button to narrow in on those selected files.

Additional toggles are available in the retractable sidebar.

As you inspect files, the app wil keep track of which files you've looked at and will move them from the "Unchecked Files" set to the "Checked Files set".
You can mark all currently plotted files as checked using the `[Check off plotted files]` button.

### Praat integration

Given a path to a praat executable (on windows) or a praat command (on Mac/Linux), as well as a directory of audio files and (optionally) textgrids, you can send files from PitchMendR to Praat.
Simply press the "Send to Praat" button and the currently opened files will be opened in Praat.
If you select a region on the plot before pressing the button, when looking at a single file, the selected region will also be sent to Praat, highlighting the same exact region.
Note that the X-axis must be on the same scale (i.e., seconds) for this to work properly.
You can press the Clear Praat Files button to automatically clear all the files in the objects pane, which is useful after loading a bunch of files.
