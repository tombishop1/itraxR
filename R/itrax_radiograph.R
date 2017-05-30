#' Import Itrax core-scanner radiograph file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param datafile defines the name of the datafile to parse
#'
#' @return a matrix of the parsed Itrax radiograph
#'
#' @examples
#' itrax_radiograph( filename=system.file( "extdata", "radiograph.tif", package = "itraxR" ) )
#'
#' @export

# reads and plots radiographs, etc.
itrax_radiograph <- function(filename, ladder=NULL, ladder_position=NULL) {

  # read the radiograph
  #require(tiff)
  rad <- tiff::readTIFF(filename)

  # define the ladder value measurements
  # this is probably quite easy to automate if you know the positions in the matrix
  ladder <- c()

  # define the "true" values for the ladder
  true_ladder <- c(1:9)

  # calibration according to the ladder values

  # optionally, map the position variable to depth

  # print an image in greyscale
  # some work needed on contrast adjustment
  # this work can be done using imagemagick::image_equalize(imagename) or maybe imagemagick::image_contrast()
  # require(lattice)
  # lattice::levelplot(df)

  # return the image file for plotting
  return(df)
}
