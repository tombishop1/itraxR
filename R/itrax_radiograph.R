##########################
### ITRAX-RADIOGRAPH #####
##########################

# reads and plots radiographs, etc.
itrax_radiograph <- function(filename, ladder=NULL, ladder_position=NULL) {

  # read the radiograph
  require(tiff)
  rad <- readTIFF(filename)

  # define the ladder value measurements
  # this is probably quite easy to automate if you know the positions in the matrix
  ladder <- c()

  # define the "true" values for the ladder
  true_ladder <- c(1:9)

  # calibration according to the ladder values

  # optionally, map the position variable to depth

  # print an image in greyscale
  # some work needed on contrast adjustment
  require(lattice)
  levelplot(df)

  # return the image file for plotting
  return(df)
}
