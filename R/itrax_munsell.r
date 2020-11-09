#' Estimate Munsell colour from an Itrax scan
#'
#' Estimates Munsell colour from a calibrated Itrax scan, or if using a Munsellator block, performs the calibration as well.
#'
#' @param datafile defines the name of the datafile to parse
#' @param scanwidth defines the swath width (in mm) to use to generate Munsell colours. This is to remove any core containers or distortion at the edges
#' @param munsellator set to TRUE if you are using a Munsellator calibration device. If not, you must supply a colour calibrated image or the output will be nonsense
#' @param intervals this is the depth-wise averaging interval. By default it parses the document.txt metadata file and sets this interval equal to the XRF scan interval, but you can supply a figure.
#'
#' @return a matrix of RGB values and munsell colours
#'
#' @examples
#' itrax_munsell("optical.tif" )
#'
#' @export

itrax_munsell <- function(filename, scanwidth=10, munsellator=FALSE, intervals="xrf"){

  require(munsell)
  require(tiff)

  # load data
  # df <- readTIFF("WV17-2-D1_RGB_16 bit.tif")
  df <- readTIFF(filename)

  # if using a Munsellator calibration block, calibrate it
  if(munsellator == TRUE){
      # this is where the calibration will be performed, using known co-ordinates
  real <- data.frame(
    R = c(94 , 241, 97 , 90 , 164, 140, 255, 7  , 255, 69 , 187, 255, 0  , 64 , 203, 255, 207, 0  , 255, 205, 152, 188, 51 , 0),
    G = c(28 , 148, 111, 103, 131, 254, 116, 47 , 0  , 0  , 255, 142, 0  , 173, 0  , 217, 3  , 148, 255, 205, 152, 118, 51 , 0),
    B = c(13 , 108, 171, 39 , 196, 153, 21 , 122, 0  , 68 , 19 , 0  , 142, 38 , 0  , 0  , 124, 189, 255, 205, 152, 118, 51 , 0)
  )

  measured <- data.frame(
    R = c(54 , 255, 81 , 60 , 173, 160, 255, 29 , 255, 38 , 195, 255, 29 , 68 , 233, 255, 255, 66 , 255, 245, 142, 94 , 29 , 12),
    G = c(16 , 123, 79 , 56 , 103, 209, 83 , 24 , 51 , 15 , 218, 119, 22 , 133, 40 , 247, 49 , 118, 255, 210, 116, 73 , 21 , 9 ),
    B = c(13 , 88 , 129, 41 , 152, 163, 46 , 49 , 26 , 23 , 114, 59 , 51 , 96 , 24 , 106, 63 , 188, 255, 232, 130, 81 , 24 , 9 )
  )
  # prepare the calibration data for the red channel
  dR <- as.data.frame(cbind(real$R, measured$R, deparse.level=2))
  colnames(dR) <- c("real", "measured")

  # model the red channel
  mR <- lm(formula = real~measured, data = dR)

  # prepare the calibration data for the green channel
  dG <- as.data.frame(cbind(real$G, measured$G, deparse.level=2))
  colnames(dG) <- c("real", "measured")

  # model the green channel
  mG <- lm(formula = real~measured, data = dG)

  # prepare the calibration data for the blue channel
  dB <- as.data.frame(cbind(real$B, measured$B, deparse.level=2))
  colnames(dB) <- c("real", "measured")

  # model the blue channel
  mB <- lm(formula = real~measured, data = dB)

  # correct the data

  } else{}

  # parse the metadata file into a list of four things:
  pixeldim <- as.numeric(itrax_meta(datafile = "document.txt")[9:11, 2])

  # make the new row names (maximum extent)
  row.names(df) <- seq(from = pixeldim[1] + (pixeldim[2] - pixeldim[1]) / (dim(df)[1]),
                  to = pixeldim[2] ,
                  by = (pixeldim[2] - pixeldim[1]) / (dim(df)[1]) )
  row.names(df) <- round(as.numeric(row.names(df)), digits=3)

  # define the intervals for depth-wise averaging
  if(intervals=="xrf"){
    # get the start, end and interval of the XRF scan in mm
    xrfdim <- as.numeric(itrax_meta(datafile = "document.txt")[6:8, 2])
    xrfdim[3] <- xrfdim[3] / 1000

    # create the scan interval points
    xrfintervals <- seq(from = xrfdim[1],
                        to = xrfdim[2],
                        by = xrfdim[3])
  } else{
    # just increment as per the variable
    xrfintervals <- seq(from = pixeldim[2],
                        to = pixeldim[3],
                        by = intervals)
  }

  # this bit will match the nearest pixel position (in mm) with the xrf scan interval (in mm)
  # a row needs adding at the beginning or the end but I'm not sure which
  avedivs <- sapply( c(1:length(xrfintervals)),
                     function(x) as.numeric(row.names(df))[which.min(abs(as.numeric(row.names(df)) - xrfintervals[x]))] )
  avedivs <- unlist(avedivs)

  # remove the head and tails of no interest
  # a row needs adding at the beginning or the end but I'm not sure which
  dfa <- df[ match(min(avedivs), as.numeric(row.names(df))) : match(max(avedivs), as.numeric(row.names(df))) , , ]

    # convert the desired sweep width in mm, into pixels
  scanwidth <- round(scanwidth/pixeldim[3], digits=0)

  # calculate the upper and lower boundaries
  widthrange <- c( round( as.numeric(dim(df)[2]) /2, digits=0 ) - (scanwidth/2),
                   round( as.numeric(dim(df)[2]) /2, digits=0 ) + (scanwidth/2))

  # average each dim of each row turnwise
  R <- apply(dfa[ , widthrange[1]:widthrange[2] , 1], 1, mean)
  G <- apply(dfa[ , widthrange[1]:widthrange[2] , 2], 1, mean)
  B <- apply(dfa[ , widthrange[1]:widthrange[2] , 3], 1, mean)

  # bind them
  dfb <- cbind(R, G, B)
  dfb <- as.data.frame(dfb)

  #### HERE IS THE RUB ####
  # average the rows by the specified depths
  R <- lapply(rep(1:length(avedivs), len=length(avedivs)-1),
                function(x) mean(dfb[match(avedivs[x], as.numeric(row.names(dfb))) : match(avedivs[x+1], as.numeric(row.names(dfb))), "R"]))

  G <- lapply(rep(1:length(avedivs), len=length(avedivs)-1),
              function(x) mean(dfb[match(avedivs[x], as.numeric(row.names(dfb))) : match(avedivs[x+1], as.numeric(row.names(dfb))), "G"]))

  B <- lapply(rep(1:length(avedivs), len=length(avedivs)-1),
              function(x) mean(dfb[match(avedivs[x], as.numeric(row.names(dfb))) : match(avedivs[x+1], as.numeric(row.names(dfb))), "B"]))

  dfc <- as.data.frame(cbind(unlist(R), unlist(G), unlist(B)))

  # make the Munsell colours
  m <- rgb2mnsl(as.matrix(dfc))

  # bind them
  # add the row headers
  dfc <- cbind(dfc, m)
  colnames(dfc) <- c("R", "G", "B")
  row.names(dfc) <- avedivs # but these are one cell too short. This will be a positioning thing, and will need a little thought.

  # maybe return an image, maybe just return the list of depths and that
  return(dfc)

}
