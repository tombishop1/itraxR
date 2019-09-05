#' Make a spectrograph from raw Itrax data spectra files
#'
#' Parses a folder full of raw spectra files from an Itrax core scanner and produces a spectral graph of all the data by position
#'
#' @param foldername defines the folder where the spectra \code{"*.spe"} files are located
#' @param datapos defines the row at which spectral data begins in the files
#'
#' @return a dataframe of all the spectral data
#'
#' @examples
#' itrax_restspectra( foldername = "XRF Data", datapos = 37 )
#'
#' @export

# function for integrating raw xrf spectra and visualising the same
itrax_restspectra <- function(foldername = "XRF data", datapos=30) {

  # read in a list of files
  filenames <- dir(foldername, pattern="*.spe")
  filenames <- paste(foldername, filenames, sep="/")

  # import them all
  require(dplyr)
  tables <- lapply(filenames, read.table, skip = datapos, header = TRUE)

  # import all the scan positions
  depths <- lapply(filenames, read.table, skip = 6, nrows = 1, header = FALSE)

  # label all the dataframes
  names(tables) <- unname(sapply(depths, `[[`, 2))

  # make an array where x=position, y=channel, and colour=intensity
  df <- unname(sapply(tables, `[[`, "content"))

  # label the cols and rows
  colnames(df) <- unname(sapply(depths, `[[`, 2))
  row.names(df) <- 1:1024 * (17.5 / 1000) # 17.5 the MCA bin width

  # draw a raster
  #require(lattice)
  #levelplot(df)

  # but really we want to use ggplot?
  #require(ggplot2)
  #melt_df <- data.table::melt(df)
  #ggplot(melt_df, aes("Var1", "Var2", z= "value")) + theme_bw() + scale_fill_gradient(low="white", high="blue")

  # return the file for plotting
  return(df)
}
