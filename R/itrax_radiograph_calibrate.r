#' Import Itrax core-scanner radiograph file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param datafile defines the name of the datafile to parse
#'
#' @return a matrix of the parsed Itrax radiograph
#'
#' @examples
#' itrax_radiograph( "radiograph.tif" )
#'
#' @export

# reads and plots radiographs, etc.
itrax_radiograph_calibrate <- function(filename, ladder=NULL, ladder_position=NULL) {

  # read the radiograph
  #require(tiff)
  rad <- tiff::readTIFF(filename)

  # define the ladder value measurements
  # this is probably quite easy to automate if you know the positions in the matrix
  ladder <- c()

  # define the "true" values for the ladder
  true_ladder <- c(1:9)




  # calibration according to the ladder values
  calibration <- data.frame(density = c(seq(1,8)),
                            greyscale = c(252,161,110,78,59,44,34,26))
  measurements <- data.frame(greyscale = c(300,100,52,1,250,24,18,150))

  model <- lm( log(density) ~ greyscale,
               data=calibration)

  measurements$density <- exp(predict(model, newdata = measurements))

  # optionally, map the position variable to depth

  # print an image in greyscale
  # some work needed on contrast adjustment
  # this work can be done using imagemagick::image_equalize(imagename) or maybe imagemagick::image_contrast()
  # require(lattice)
  # lattice::levelplot(df)

  # return the image file for plotting
  return(df)
}
