#' Import Itrax core-scanner RGB image file
#'
#' Imports and parses an image file from an Itrax core-scanner.
#'
#' @param filename defines the name of the datafile to parse
#'
#' @return a matrix of the parsed Itrax image
#'
#' @examples
#' itrax_image( filename=system.file( "extdata", "optical.tif", package = "itraxR" ) )
#'
#' @export


# reads and plots optical line scan images
itrax_image <- function(filename) {

  # read the image
  require(tiff)
  img <- tiff::readTIFF(filename)

  # map the image to coring depth (optional?)

  # make contrast adjustments as appropriate

  # return the image file for plotting
  return(df)
}
