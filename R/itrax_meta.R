#' Parse Itrax scan metadata
#'
#' Parses the \code{"document.txt files"} generated from Itrax core scanners
#'
#' @param datafile a \code{"document.txt files"} generated from an Itrax core scanner
#'
#' @importFrom utils read.table
#'
#' @return a dataframe of all the parsed input data
#'
#' @examples
#' \dontrun{itrax_meta("document.txt")}
#'
#' @export

# Function takes a document.txt file and parses it into a sensible format.

itrax_meta=function(datafile="document.txt"){

  # assert file exists
  if (!file.exists(datafile)){
    stop('File doesn\'t exist, check your working directory.')
  } else{}

  # import the file
  paths <- read.table(datafile, header = TRUE, sep = "\t", nrows = 1)
  dates <- read.table(datafile, header = TRUE, sep = "\t", nrows = 1, skip = 2)
  parameters <- read.table(datafile, header = FALSE, sep = "\t", skip = 4, stringsAsFactors = FALSE)

  # housekeeping
  parameters[ , 1] <- as.character(parameters[ , 1])
  parameters[ , 3] <- as.character(parameters[ , 3])

  # move those ones that aren't in the right place
  stoprow <- c(parameters[8,3], as.numeric(parameters[8,4]), NA, NA)
  names(stoprow) <- colnames(parameters)
  xrfcurrent <- c("XRF current", parameters[9,4], NA, NA)
  names(xrfcurrent) <- colnames(parameters)
  opticalend <- c(parameters[11,3], parameters[11,4], NA, NA)
  names(opticalend) <- colnames(parameters)
  parameters <- rbind(parameters, stoprow, xrfcurrent, opticalend)

  # re-label the columns
  colnames(parameters) <- c("Parameter", "Value", "Unit")

  # cleanup and rename some things
  parameters[ , 4] <- NULL
  parameters[6, 3] <- "ON/OFF"
  parameters[9, 3] <- "kV"
  parameters[9, 1] <- "XRF voltage"
  parameters[10,3] <- "element"
  parameters[8, 3] <- "mm"
  parameters[19,3] <- "mm"
  parameters[20,3] <- "mA"
  parameters[21,3] <- "mm"
  parameters[11,3] <- "mm"
  parameters[12,3] <- "mm"  #yes I know it is labelled as microns in the file, but it is wrong!
  parameters[1,1 ] <- "Rad. voltage"
  parameters[2,1 ] <- "Rad. current"
  parameters[3,1 ] <- "Rad. exposure"
  parameters[12,1] <- "Optical step size"

  # add the date
  date <- paste( as.character(dates[1,3]),"/",as.character(dates[1,2]),"/",as.character(dates[1,1]), sep="" )
  daterow <- c("Aquisition date", date, "dd/mm/yyyy")
  names(daterow) <- colnames(parameters)
  parameters <- rbind(parameters, daterow)

  # add the names
  samplename <- c("Sample name", as.character(paths[1,2]), "str")
  names(samplename) <- colnames(parameters)
  sectionname <- c("Section name", as.character(paths[1,3]), "str")
  names(sectionname) <- colnames(parameters)
  operatorname <- c("Operator name", as.character(paths[1,4]), "str")
  names(operatorname) <- colnames(parameters)

  parameters <- rbind(parameters, samplename, sectionname, operatorname)

  # Sort it all into a sensible order
  sortingorder <- c( "Sample name", "Section name", "Aquisition date", "Operator name",
                     "Tube", "Start coordinate", "Stop coordinate", "Step size",
                     "Optical Start", "Optical End", "Optical step size",
                     "Rad. voltage", "Rad. current", "Rad. exposure", "line camera signal level",
                     "XRF", "XRF voltage", "XRF current", "XRF exp. time",
                     "Start temperature", "Stop temperature", "Start humidity", "Stop humidity", "Start vacuum", "Stop vacuum" )

  parameters <- as.data.frame(parameters[match(sortingorder, parameters$Parameter),], stringsAsFactors = TRUE)
  rownames(parameters) <- NULL

  # pass the data
  return(parameters)
}
