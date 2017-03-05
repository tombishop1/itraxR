##########################
###### ITRAX-IMAGE #######
##########################

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
