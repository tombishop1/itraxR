#' Estimate Munsell colour from an Itrax scan
#'
#' Estimates Munsell colour from a calibrated Itrax scan, or if using a Munsellator block, performs the calibration as well.
#'
#' @param filename defines the name of the datafile to parse
#' @param metadata defines the relating metadata
#'
#' @return a matrix of RGB values
#'
#' @examples
#' itrax_image()
#'
#' @export
#'
itrax_image <- function(filename, metadata = "document.txt") {

  # load the image
  require(tiff)
  df <- readTIFF(filename)

  # load the metadata
  metadata <- itrax_meta(metadata)

  # generate the positions on the row
  row.names(df) <- seq(from=0, to=as.numeric(metadata[10,2]), by=as.numeric(metadata[11,2]))

  # generate the return
  return(df)
}
