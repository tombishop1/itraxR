#' Import Itrax core-scanner radiograph file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param datafile defines the name of the datafile to parse
#'
#' @return a matrix of the parsed Itrax radiograph
#'
#' @examples
#' itrax_radiograph()
#'
#' @export

# reads and plots radiographs, etc.
itrax_radiograph <- function(filename = "radiograph.raw", metadata = "document.txt") {

  # read the radiograph
  df <- t(read.delim(filename, header = FALSE))

  # read the metadata
  metadata <- itrax_meta(metadata)

  # generate the row names
  row.names(df) <- seq(from=as.numeric(metadata[6,2]), to=as.numeric(metadata[7,2]), by=as.numeric(metadata[8,2])/1000)

  # generate the row names
  colnames(df) <- seq(from=0.02, to=16, by=0.02) # because one pixel is 20 microns

  #generate the return object
  return(df)
}
