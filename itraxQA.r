itrax_spectracompare=function(filea, fileb, datapos=32, graph=TRUE) {

# check the files exist
if (!file.exists(filea) | !file.exists(fileb)) {
  stop('File doesn\'t exist, check your working directory.')
  return()
}

# load the files into dataframes
dfa <- read.table(filea, skip = datapos, header = TRUE)
dfb <- read.table(fileb, skip = datapos, header = TRUE)

dfa_meta <- read.table(filea, nrows = datapos, header = FALSE, sep = "\t")
dfb_meta <- read.table(fileb, nrows = datapos, header = FALSE, sep = "\t")

# check for any parameter mis-matches
#if (! x == x | ... ) {
#	print("There are parameter mis-matches!")
#	return()
#}

# check the file for the mca_bin_width parameter (here, fixed at 19.26, but that doesn't seen quite right...)

# convert the channels into energies (kEV)
dfa[ , 3] <- ( dfa[ , 1] * 19.26 ) / 1000
dfb[ , 3] <- ( dfb[ , 1] * 19.26 ) / 1000

# subtract one spectra from the other
dfc <- dfb
dfc[ , 2] <- dfb[ , 2] - dfa[ , 2]

if (graph==TRUE) {
	# plot both spectra against one another
	plot(dfa[ , 3], dfa[ , 2], type="l", col="blue", log="y", xlab = "Energy (kEV)", ylab = "Counts")
	par(new=TRUE)
	plot(dfb[ , 3], dfb[ , 2], type="l", col="red", log="y", axes=FALSE, xlab="", ylab="")
	axis(1, at=c(1:20))

	# add the Cr, Mo and W energy lines
	energynames=list("Cr", "Cr", "Mo", "Mo", "W", "W")
	energylines=list(5.411, 0.573, 17.441, 2.293, 8.396, 1.774)
	abline(v = energylines, col = "pink")

	# add labels to those lines
	maxy <- max(dfa[ , 2]) + 10
	text(x = energylines, y = maxy, labels = energynames, col = "pink", cex = 0.75)	
	
	# plot the residuals
	dev.new()
	plot(dfc[ , 3], dfc[ , 2], type="l", xlab="Energy (kEV)", ylab="Difference (counts)", main="Difference in spectra")

	# add the Cr, Mo and W energy lines
	abline(v = energylines, col = "red")

	# add labels to those lines
	maxy <- max(dfc[ , 2]) + 10
	text(x = energylines, y = maxy, labels = energynames, col = "red", cex = 0.75)
}

return(dfc)
}