% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pitch.read.R
\name{pitch.read2}
\alias{pitch.read2}
\title{pitch.read2}
\usage{
pitch.read2(fileNamePitch, encoding = "UTF-8", f0_only = FALSE)
}
\arguments{
\item{fileNamePitch}{file name of Pitch object}

\item{encoding}{File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding}

\item{f0_only}{Logical, if TRUE will use a faster routine to just load the first candidate from each frame}
}
\value{
A Pitch object represents periodicity candidates as a function of time.
}
\description{
Reads Pitch object from Praat. Modified from the function in \code{{rPraat}}.
Supported formats: text file, short text file.
}
\examples{
\dontrun{
p <- pitch.read2('demo/sound.Pitch')
names(p)
p$nx
p$t[4]        # time instance of the 4th frame
p$frame[[4]]  # 4th frame: pitch candidates
p$frame[[4]]$frequency[2]
p$frame[[4]]$strength[2]
}
}
