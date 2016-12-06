# Script to read in Itrax result files. 
# The result file will usually be named result.txt or results.txt

# pass the current working directory to the user
message("Current working directory is ", getwd())

# ascertain the results file to load from the user
filename <- readline(prompt = "Enter filename of results file, or enter q to quit: ")

# check if the user has chosen to quit, or return the filename entered. Secret shortcut for result.txt (r)
if(filename == "q") {
	stop("Ending the script due to user input.")
} else if(filename == "r") {
	filename <- "result.txt" 
	message("Filename is set to: ", filename)
} else {
	message("Filename is set to: ", filename)
}

# read the result file into a dataframe
df <- read.table(filename, skip = 2, header = TRUE, sep = "\t")

# remove variables of no interest - this could be modified to only include specified variables using "df[grep(..."
df <- df[-grep("filename|E.gain|E.offset|F.slope|F.offset|X|Fe.a.2|Fe.a.b|W.La|W.Lb1|W.lb2|W.La.in|W.Lb1.in|W.Lb2.in|Dt",colnames(df))]

# re-name the position variable
colnames(df)[which(names(df) == "position..mm.")] <- "position"

# convert the binary values in validity to true/false
df$validity <- as.logical(df$validity)

# convert position into an numeric value
df$position <- as.numeric(df$position)

# display some information about what just got loaded in
message("Loaded ", nrow(df), " rows of data")

# ask for information to re-map depths of data
depth_top <- readline(prompt = "Enter the true drilling depth of the top of the core in mm: ")
depth_top <- as.numeric(depth_top)

# display the offset between the position and drilling depth at the top of the core
message("The offset between the position and the true drilling depth is ", df[1, "position"] - depth_top)

# calculate the new depth variable
depth_offset <- depth_top - df[1, "position"]
df$depth <- df$position + depth_offset

# make a pretty graph
require(analogue)
Stratiplot(df[-grep("depth|position",colnames(df))], df$depth, varTypes = "absolute", absolutesize = "1", strip = TRUE)

# ask for the sub-sampling interval for averaging into chunks (binning)
ai <- readline(prompt = "Enter the sampling interval to use in binning operations in mm: ")
ai <- as.numeric(ai)

# work out the distance as row on row increment represents
scaninterval <- df[2,"depth"] - df[1,"depth"]
scaninterval <- round(scaninterval, digits =2)

# change ai to represent the equivalent number of rows
# round it so it is a whole number
ai <- ai / scaninterval
ai <- round(ai)
message("The scan interval is ", scaninterval , ", and so the binning window has been rounded to ", round(ai * scaninterval) ,"mm.")

# ask for where the junk data becomes good data for the purposes of binning
binning_start <- readline(prompt = "Enter the start depth of good data in mm: ")
binning_start <- as.numeric(binning_start)

# trim the junk data at the beginning of the scan
df <- subset(df, df$depth > binning_start)

#trim the junk data at the end of the scan
binning_end <- readline(prompt = "Enter the end depth of good data in mm: ")
binning_end <- as.numeric(binning_end)

# trim the junk data at the end of the scan
df <- subset(df, df$depth < binning_end)

# perform the binning operation
df_avg <- aggregate(df,list(rep(1:(length(df$depth)%/%ai+1),each=ai,len=length(df$depth))),mean)[-1]
df_avg$depthmin <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%ai+1),each=ai,len=length(df$depth))),min)$x
df_avg$depthmax <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%ai+1),each=ai,len=length(df$depth))),max)$x

# load and prepare the data for the PCA
df_clr <- df
rownames(df_clr) <- round(df_clr$depth, digits = 1)
df_clr <- df_clr[-grep("position|sample.surface|validity|cps|MSE|Mo.inc|Mo.coh|depth",colnames(df_clr))]

# calculate centered log ratios for all elements in the original dataset
require(compositions)
#clr(...)

# perform a principle components analysis of the original dataset and display the eigenvalues
#princomp(...)

# plot an ordination diagram of axes 1 and 2, with sample and variable scores, to explore the data (new window)

# offer the option to export the data
export_opt <- readline(prompt = "Do you want to export the data produced by this script? (y/n) ")
if(export_opt == "y") {
	write.table(df_avg, "results_avg.txt", sep="\t")
} else {
	message("No data export initiated")
}

# offer the option to clean-up the workspace
message("The following objects are in memory:")
ls()
cleanup_opt <- readline(prompt = "Do you wish to clean-up (e)verything, (n)othing, or (l)eave only the data: ")
if(cleanup_opt == "e") {
	rm(list = ls())
	message("The following objects are now in memory:")
	message(ls())
} else if(cleanup_opt == "l") {
	rm("ai", "depth_offset", "depth_top", "filename", "cleanup_opt", "export_opt", "scaninterval", "binning_start")
	message("The following objects are now in memory:")
	message(ls())
} else {
	message("The following objects are now in memory:")
	message(ls())
}

# end the script gracefully
stop("The script ends here.")