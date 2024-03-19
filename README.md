# PitchMendR

This package provides a shiny app used to modify F0 pitch pulses held in a spreadsheet.

## Citation

If you use this software in your research, please cite the following paper:

Sostarics, T., & Cole, J. (2024). *PitchMendR: A Tool for the Diagnosis and Treatment of F0 Tracking Errors*. In Proceedings of Speech Prosody 2024.

Bibtex:

```
@inproceedings{sostarics2024sp,
author = {Sostarics, Thomas and Cole, Jennifer},
year = {2024},
month = {6},
title = {{PitchMendR}: A Tool for the Diagnosis and Treatment of {F0} Tracking Errors},
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
The app assumes the following columns exist by default:

 - A `Filename` column specifying the individual pitch contours
 - A `t_ms` column to use as the x-axis timestamp values for the pulses
 - A `f0` column to use as the y-axis values

These are the default outputs from PraatSauce, which exports a single csv file containing information about multiple processed files.
If any of these columns are not found in the dataset, you'll be brought to the `Settings` page where you can change the column name mappings.
Once you've set the column names correctly you can try loading the file again.

### Editor page

There are three main sections, from right to left::

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

 - A point size slider: changes the size of plotted pulses
 - A transparency slider: Changes the transparency of the plotted pulses.
 - Regex text input: Type a regular expression and press `[Plot matches]` to plot only those files whose filenames match the pattern. Set to `.` to show all files.
 - Number of files plotted: reflects number of files plotted
 - Brushed files: When you select pulses, the files those pulses belong to will be displayed. You can click the `[Plot Brushed Files]` button to narrow in on those selected files.

### Tools Sidebar information

As you inspect files, they will be moved from the set of unchecked files to the set of checked files.
You can mark all plotted files as checked using the `[Check off plotted files]` button.
