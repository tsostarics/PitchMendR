#' pitch.read2
#'
#' Reads Pitch object from Praat. Modified from the function in `{rPraat}`.
#' Supported formats: text file, short text file.
#'
#' @param fileNamePitch file name of Pitch object
#' @param encoding File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding
#'
#' @return A Pitch object represents periodicity candidates as a function of time.
#' @return   [ref: Praat help, https://www.fon.hum.uva.nl/praat/manual/Pitch.html]
#' @return   \code{p$xmin} ... start time (seconds)
#' @return   \code{p$xmax} ... end time (seconds)
#' @return   \code{p$nx}   ... number of frames
#' @return   \code{p$dx}   ... time step = frame duration (seconds)
#' @return   \code{p$x1}   ... time associated with the first frame (seconds)
#' @return   \code{p$t}    ... vector of time instances associated with all frames
#' @return   \code{p$ceiling}        ... a frequency above which a candidate is considered voiceless (Hz)
#' @return   \code{p$maxnCandidates} ... maximum number of candidates in frame
#' @return   \code{p$frame[[1]]} to \code{p$frame[[p$nx]]} ... frames
#' @return      \code{p$frame[[1]]$intensity}   ... intensity of the frame
#' @return      \code{p$frame[[1]]$nCandidates} ... actual number of candidates in this frame
#' @return      \code{p$frame[[1]]$frequency} ... vector of candidates' frequency (in Hz)
#' @return                               (for a voiced candidate), or \code{0} (for an unvoiced candidate)
#' @return      \code{p$frame[[1]]$strength}  ... vector of degrees of periodicity of candidates (between \code{0} and \code{1})
#'
#' @importFrom rPraat str_contains
#' @examples
#' \dontrun{
#' p <- pitch.read2('demo/sound.Pitch')
#' names(p)
#' p$nx
#' p$t[4]        # time instance of the 4th frame
#' p$frame[[4]]  # 4th frame: pitch candidates
#' p$frame[[4]]$frequency[2]
#' p$frame[[4]]$strength[2]
#' }
pitch.read2 <- function(fileNamePitch, encoding = "UTF-8") {
  if (!isString(fileNamePitch)) {
    stop("Invalid 'fileNamePitch' parameter.")
  }

  if (!isString(encoding)) {
    stop("Invalid 'encoding' parameter.")
  }
  enc <- encoding

  if (encoding == "auto") {
    enc <- detectEncoding(fileNamePitch)
  }

  if (enc == "UTF-8") {
    flines <- readr::read_lines(fileNamePitch, locale = readr::locale(encoding = "UTF-8"))  # Does not support UTF-16 at this point :-(
  } else {
    fid <- file(fileNamePitch, open = "r", encoding = enc)
    flines <- readLines(fid)   # does not work with tests/testthat/utf8.TextGrid  :-(
    close(fid)
  }

  flines <- enc2utf8(flines)

  if (length(flines) < 1) {
    stop(paste0("Empty file: ", fileNamePitch))
  }

  if (encoding == "UTF-8" & flines[1] != 'File type = "ooTextFile"') {
    warning('Not an UTF-8 Pitch format, trying encoding = "auto"...')
    x <- pitch.read2(fileNamePitch, encoding = "auto")
    return(x)
  }

  pitch_ind <- pitch.read_lines2(flines)
  class(pitch_ind[[1]])["type"] <- "Pitch 1"
  class(pitch_ind[[1]])["name"] <- basename(fileNamePitch)
  return(pitch_ind[[1]])
}

# Modified from the function in `{rPraat}`. Changes:
# - Double indices -> integer indices
# - Use .Internal gsub (strTrim2) to avoid factor check
# - Avoid repetitive use of strTrim
# - Use seq.int instead of seqM
# - Use .Internal substr to avoid unnecessary character coercion
#     note that the C code loop checks i <= stop && str < end, so although
#     you could pass a single large value to avoid calling nchar(), the
#     additional iterations won't short circuit and end up being slightly slower
# - Cut back on number of addition operations
# - Use sprintf for string interpolation instead of paste0
pitch.read_lines2 <- function(flines, find = 1L, collection = FALSE) {
  if (collection  ||  flines[find-1L+ 1L] == "File type = \"ooTextFile\"") {    # TextFile or shortTextFile
    if (!collection) {
      if (length(flines)-find+1L < 11L) {
        stop("Unknown Pitch format.")
      }

      if (flines[find-1L+ 2L] != "Object class = \"Pitch 1\"") {
        stop("Unknown Pitch format.")
      }

      if (flines[find-1L+ 3L] != "") {
        stop("Unknown Pitch format.")
      }

      if (nchar(flines[find-1L+ 4L]) < 1L) {
        stop("Unknown Pitch format.")
      }
    } else {
      find <- find - 3
    }
    find <- find-1L
    if (str_contains(flines[find+ 4], "xmin")) {  # TextFile
      xmin_str  <- strTrim2(flines[find+ 4L])
      xmax_str  <- strTrim2(flines[find+ 5L])
      nx_str    <- strTrim2(flines[find+ 6L])
      dx_str    <- strTrim2(flines[find+ 7L])
      x1_str    <- strTrim2(flines[find+ 8L])
      ceil_str  <- strTrim2(flines[find+ 9L])
      ncand_str <- strTrim2(flines[find+ 10L])
      xmin <- as.numeric(          .Internal(substr(xmin_str,   8L, nchar(xmin_str)))) # first digit at 8, then the rest
      xmax <- as.numeric(          .Internal(substr(xmax_str,   8L, nchar(xmax_str))))
      nx <- as.integer(            .Internal(substr(nx_str,     6L, nchar(nx_str))))
      dx <- as.numeric(            .Internal(substr(dx_str,     6L, nchar(dx_str))))
      x1 <- as.numeric(            .Internal(substr(x1_str,     6L, nchar(x1_str))))
      ceil <- as.numeric(          .Internal(substr(ceil_str,  11L, nchar(ceil_str))))
      maxnCandidates <- as.integer(.Internal(substr(ncand_str, 18L, nchar(ncand_str))))

      frame <- vector("list", nx)

      if (!str_contains(flines[find+ 11L], "frame []: ") & !str_contains(flines[find+ 11L], "frames []: ")) {
        stop("Unknown Pitch format.")
      }

      iline <- find+ 12L  # index of line to read

      for (I in seq.int(1L, nx)) {
        cur_line <- strTrim2(flines[iline])
        # will usually short circuit on the first expression
        if (cur_line != sprintf("frames [%d]:", I) & cur_line != sprintf("frame [%d]:", I)) {
          stop(paste0("Unknown Pitch format, wrong frame id (", I, "')."))
        }
        iline <- iline + 1L
        cur_line <- strTrim2(flines[iline])

        intensity <- as.numeric(.Internal(substr(cur_line, 13L, nchar(cur_line)))); iline <- iline + 1L
        cur_line <- strTrim2(flines[iline])

        nCandidates <- as.integer(.Internal(substr(cur_line, 15L, nchar(cur_line)))); iline <- iline + 1L

        if (!str_contains(flines[iline], "candidates []:") & !str_contains(flines[iline], "candidate []:")) {
          stop("Unknown Pitch format.")
        }
        iline <- iline + 1L

        frequency <- numeric(nCandidates)
        strength  <- numeric(nCandidates)


        for (Ic in seq.int(1L, nCandidates)) {
          cur_line <- strTrim2(flines[iline])

          if (cur_line != sprintf("candidate [%d]:", Ic) & cur_line != sprintf("candidates [%d]:", Ic)) {
            stop(paste0("Unknown Pitch format, wrong candidate nr. (", Ic, ") in frame id (", I, "')."))
          }
          iline <- iline + 1L
          cur_line <- strTrim2(flines[iline])

          nmbr <- .Internal(substr(cur_line, 13L, nchar(cur_line))); iline <- iline + 1L
          if (nmbr != "--undefined--") {
            frequency[Ic] <- as.numeric(nmbr)
          } else {
            frequency[Ic] <- NA_real_
          }

          cur_line <- strTrim2(flines[iline])
          nmbr <- .Internal(substr(cur_line, 12L, nchar(cur_line))); iline <- iline + 1L
          if (nmbr != "--undefined--") {
            strength[Ic] <-  as.numeric(nmbr)
          } else {
            strength[Ic] <-  NA_real_
          }
        }

        frame[[I]] <- list(intensity = intensity, nCandidates = nCandidates,
                           frequency = frequency, strength =  strength)
      }

    } else {   # shortTextFile
      xmin <- as.numeric(          flines[find+ 4L])
      xmax <- as.numeric(          flines[find+ 5L])
      nx <- as.integer(            flines[find+ 6L])
      dx <- as.numeric(            flines[find+ 7L])
      x1 <- as.numeric(            flines[find+ 8L])
      ceil <- as.numeric(          flines[find+ 9L])
      maxnCandidates <- as.integer(flines[find+ 10L])

      frame <- vector("list", nx)

      iline <- find+ 11L  # index of line to read

      for (I in seq.int(1L, nx)) {
        intensity <- as.numeric(flines[iline]); iline <- iline + 1L
        nCandidates <- as.integer(flines[iline]); iline <- iline + 1L

        frequency <- numeric(nCandidates)
        strength <- numeric(nCandidates)

        for (Ic in seq.int(1L, nCandidates)) {
          nmbr <- flines[iline]; iline <- iline + 1L
          if (nmbr != "--undefined--") {
            frequency[Ic] <- as.numeric(nmbr)
          } else {
            frequency[Ic] <- NA_real_
          }

          nmbr <- flines[iline]; iline <- iline + 1L
          if (nmbr != "--undefined--") {
            strength[Ic] <- as.numeric(nmbr)
          } else {
            strength[Ic] <- NA_real_
          }
        }

        frame[[I]] <- list(intensity = intensity, nCandidates = nCandidates,
                           frequency = frequency, strength =  strength)
      }
    }

  } else {   # unknown format
    stop("Unknown Pitch format.")
  }


  p <- list(xmin = xmin, xmax = xmax, nx = nx, dx = dx, x1 = x1, t = seq.int(0L, (nx-1L))*dx + x1,
            ceiling = ceil, maxnCandidates = maxnCandidates,
            frame = frame)

  return(list(p, iline))
}

# Modified from the function in `{rPraat}` to skip the factor check in gsub
strTrim2 <- function(x) .Internal(gsub("^\\s+|\\s+$", "", x, FALSE, FALSE, FALSE, FALSE))

# Modified from rPraat
#' @export
isString <- function(string) {
  (inherits(string, "character"))  &
    (length(string) == 1L) &
    (!is.na(string))
}
