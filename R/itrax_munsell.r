#' Estimate Munsell colour from an Itrax scan
#'
#' Estimates Munsell colour from a calibrated Itrax scan, or if using a Munsellator block, performs the calibration as well.
#'
#' @param datafile defines the name of the datafile to parse
#' @param scanwidth defines the swath width to use to generate Munsell colours. This is to remove any core containers or distortion at the edges
#' @param munsellator set to TRUE if you are using a Munsellator calibration device. If not, you must supply a colour calibrated image or the output will be nonsense
#' @param intervals this is the depth-wise averaging interval. By default it parses the document.txt metadata file and sets this interval equal to the XRF scan interval, but you can supply a list.
#'
#' @return a matrix of RGB values and munsell colours
#'
#' @examples
#' itrax_munsell( filename=system.file( "extdata", "optical.tif", package = "itraxR" ) )
#'
#' @export

itrax_munsell <- function(filename, scanwidth=30, munsellator=FALSE, intervals="xrf"){

  require(munsell)
  require(tiff)

  # load data
  # df <- readTIFF("WV17-2-D1_RGB_16 bit.tif")
  df <- readTIFF(filename)

  # if using a Munsellator calibration block, calibrate it
  if(munsellator == TRUE){
    # this is where the calibration will be performed, using known co-ordinates
  } else{}

  # parse the metadata file into a list of four things:
  # [ 1 ] is the step size in mm
  # [ 2 ] is the optical scan start position in mm
  # [ 3 ] is the optical scan end position in mm
  # [ 4 ] is the optical step size in mm
  pixeldim <- as.numeric(itrax_meta(datafile = "document.txt")[8:11, 2])
  # pixeldim[1] <- pixeldim[1] / 1000
  # pixeldim[4] <- pixeldim[4] / 1000

  # work out the pixels per unit mm
  # [ 5 ] is the number of pixels per unit mm of length
  # pixeldim[5] <- dim(df)[2] / ( pixeldim[3] - pixeldim[2] )

  # make a list of the row names
  rowmarks <- seq(from = pixeldim[2] + (pixeldim[3] - pixeldim[2]) / (dim(df)[1]), to = pixeldim[3] , by = (pixeldim[3] - pixeldim[2]) / (dim(df)[1]) )

  # make the row names equal to position
  row.names(df) <- rowmarks

  # define the intervals for depth-wise averaging
  if(intervals=="xrf"){
    # get the start, end and interval of the XRF scan
    xrfdim <- as.numeric(itrax_meta(datafile = "document.txt")[6:8, 2])

    # create the scan interval points
    xrfintervals <- seq(from = xrfdim[1], to = xrfdim[2], by = xrfdim[3]/1000)
  } else{}

  # average the depths by the intervals
  dfa <- df

  # get the sweep width co-ordinates
  width <- as.numeric(dim(df)[1])
  midpoint <- round(width/2, digits=0)
  lowbound <- midpoint-(scanwidth/2)
  highbound <- midpoint+(scanwidth/2)

  # average each dim of each row turnwise
  R <- apply(dfa[ , lowbound:highbound , 1], 1, mean)
  G <- apply(dfa[ , lowbound:highbound , 2], 1, mean)
  B <- apply(dfa[ , lowbound:highbound , 3], 1, mean)

  # bind them
  dfb <- cbind(R, G, B)

  # make the Munsell colours
  m <- rgb2mnsl(as.matrix(dfb))

  # bind them
  # add the row headers
  dfc <- cbind(R, G, B, m)

  # maybe return an image, maybe just return the list of depths and that
  return(dfc)

}

