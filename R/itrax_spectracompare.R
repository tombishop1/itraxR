#' Import Itrax core-scanner spectra file comparison
#'
#' Imports, parses and compares two Itrax spectra files, used for Q.C. on Itrax calibration blocks.
#'
#' @param filea defines the name of the first file
#' @param fileb defines the name of the second file
#' @param datapos defines the line of the input files at which the metadata ends and the spectral data begins.
#' @param graph binary operator that if TRUE will produce a graph of the two spectra
#'
#' @return returns a dataframe of the difference between the two
#'
#' @examples
#' itrax_spectracompare(filea = system.file("extdata", "a.spe", package = "itraxR"), fileb = system.file("extdata", "b.spe", package = "itraxR"))
#'
#' @export

itrax_spectracompare=function(filea, fileb, datapos=37, graph=TRUE) {
  # Compares two spectra - filea and fileb

  # check the files exist
  if (!file.exists(filea) | !file.exists(fileb)) {
    stop('File doesn\'t exist, check your working directory.')
  }

  # load the files into dataframes
  dfa <- read.table(filea, skip = datapos, header = TRUE)
  dfb <- read.table(fileb, skip = datapos, header = TRUE)

  dfa_meta <- read.table(filea, nrows = datapos, header = FALSE, sep = "\t", stringsAsFactors=FALSE)
  dfb_meta <- read.table(fileb, nrows = datapos, header = FALSE, sep = "\t", stringsAsFactors=FALSE)

  # tidy up the metadata
  row.names(dfa_meta) <- dfa_meta[ , 1]
  dfa_meta[ , 1] <- NULL
  dfa_meta[ , 3] <- NULL
  dfa_date <- paste(dfa_meta[1,1], dfa_meta[2,1])
  print(dfa_meta)

  row.names(dfb_meta) <- dfb_meta[ , 1]
  dfb_meta[ , 1] <- NULL
  dfb_meta[ , 3] <- NULL
  dfb_date <- paste(dfb_meta[1,1], dfb_meta[2,1])

  # check for any parameter mis-matches in anode, voltage or current
  runtime_a <- round(as.numeric(dfa_meta["runtime", 1]))
  runtime_b <- round(as.numeric(dfb_meta["runtime", 1]))
  if (! dfa_meta["Tube anode:", 1]  == dfb_meta["Tube anode:", 1]
      | ! dfa_meta["Tube current:", 1]  == dfb_meta["Tube current:", 1]
      | ! dfa_meta["Tube voltage:", 1]  == dfb_meta["Tube voltage:", 1]
      | ! runtime_a ==  runtime_b ){
    warning("There are parameter mis-matches!")
  } else{}

  # check the key/channel in metadata match, then pass here
  if(as.numeric(dfa_meta["mca_bin_width", 1]) == as.numeric(dfb_meta["mca_bin_width", 1]) ) {
    kev_channel <- as.numeric(dfa_meta["mca_bin_width", 1])
    kev_channel <- kev_channel / 1000 # convert to KeV
  } else { stop('There is a bin width mis-match.')  }
  # kev_channel <- 0.0175 ...normally

  # convert the channels into energies (kEV)
  dfa[ , 3] <- dfa$channel * kev_channel
  dfb[ , 3] <- dfb$channel * kev_channel

  # subtract one spectra from the other
  dfc <- dfb
  dfc[ , 2] <- dfb[ , 2] - dfa[ , 2]

  if (graph==TRUE) {
    # plot both spectra against one another
    # this should be done in ggplot
    plot(dfa[ , 3], dfa[ , 2], type="l", col="blue", log="y", xlab = "Energy (keV)", ylab = "Counts")
    par(new=TRUE)
    plot(dfb[ , 3], dfb[ , 2], type="l", col="red", log="y", axes=FALSE, xlab="", ylab="")
    axis(1, at=c(1:20))

    # add the Cr, Mo and W energy lines
    energynames=list("Cr Ka", "Cr Ka", "Cr Kb1", "Mo Ka", "Mo Ka", "Mo Kb", "W La1"  )
    energylines=list(5.41472, 5.405509, 5.95671, 17.47934, 17.3743, 19.6083, 8.3976)
    #energynames=list("Fe", "Fe", "Fe", "Cr")
    #energylines=list(6.40384, 6.39084, 7.0579, 5.411)
    abline(v = energylines, col = "pink")

    # add labels to those lines
    maxy <- max(dfa[ , 2]) + 10
    text(x = energylines, y = maxy, labels = energynames, col = "pink", cex = 0.75)

    # plot the residuals
    # dev.new()
    # plot(dfc[ , 3], dfc[ , 2], type="l", xlab="Energy (kEV)", ylab="Difference (counts)", main="Difference in spectra")

    # add the Cr, Mo and W energy lines
    # abline(v = energylines, col = "red")

    # add labels to those lines
    # maxy <- max(dfc[ , 2]) + 10
    # text(x = energylines, y = maxy, labels = energynames, col = "red", cex = 0.75)
  }

  return(dfc)
}
