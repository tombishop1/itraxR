# itraxR --- Functions to manipulate Itrax data files

###################
## ITRAX-IMPORT  ##
###################

itrax_import=function(datafile="result.txt", depth_top=NULL, trim_top=NULL, trim_bottom=NULL, na_validity=TRUE, 
                      parameters="SOME", graph=FALSE, export=NULL) {
  
# datafile    pass the name of the datafile you want to parse.
# depth_top   pass he coring depth of the sequence in mm.
# trim_top    pass how much of the top of the core to remove in mm. combine with negative depth_top if required.
# trim_bottom pass how much of the bottom of the core to remove in mm.
# na_validity sets all invalid measurements to NA.
# parameters  use ALL, SOME or ELEMENTS to select which data to include. 
# graph       TRUE or FALSE to plot a stratigraphic diagram. 
# export      pass the name of file you want to export the data to, if you want to export.


# assert file exists
if (!file.exists(datafile)){
  stop('File doesn\'t exist, check your working directory.')
} else{}

# read the result file into a dataframe
df = read.table(datafile, skip = 2, header = TRUE, sep = "\t")

# select only variables of interest
# first, generate some lists
# a list of element names
elements = c("H", "He",
"Li", "Be", "B", "C", "N", "O", "F", "Ne",
"Na", "Mg", "Al", "Si", "P", "S", "Cl", 
# "Ar",
"K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
"Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
"Cs", "Ba", 
"La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", 
"Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn",
"Fr", "Ra", 
"Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", 
"Lr", "Unq", "Unp", "Unh", "Uns", "Uno", "Une", "Unn")

# a list of other useful parameters
others = c("position..mm.", "sample.surface", "MSE", "cps", "validity")

# remove parameters that aren't of interest
if(parameters=="SOME"){
	df <- df[ , which(names(df) %in% append(elements, others))]
} else if(parameters=="ALL"){
	df <- df
} else if(parameters=="ELEMENTS"){
  df <- df[ , which(names(df) %in% append(elements, append(others[[1]], others[[5]])))]
} else {
warning('The parameters variable must be SOME, ALL or ELEMENTS.')
}

# re-name the position variable
colnames(df)[which(names(df) == "position..mm.")] <- "position"

# convert position into an numeric value
df$position = as.numeric(df$position)

# calculate the new depth variable
if(!is.null(depth_top)){
	depth_offset = depth_top - df[1, "position"]
	df$depth = df$position + depth_offset
	rownames(df) <- round(df$depth, digits = 1)
} else{}

# trim the junk data at the beginning of the scan

# establish whether depth or position is being used
if(!is.null(depth_top)){
  x <- df$depth
} else{x <- df$position}

if(!is.null(trim_top)){
	top_trim_position <- min(x) + trim_top
	df <- subset(df, x > top_trim_position)
} 

# trim the junk data at the end of the scan
if(!is.null(trim_bottom)){
	bottom_trim_position <- max(x) - trim_bottom 
	df <- subset(df, x < bottom_trim_position)
}

# Replace all rows with validity == 0 with NA
if(na_validity==TRUE){
  
  position_temp <- df$position
  
  if(!is.null(depth_top)){
    depth_temp <- df$depth
  } else{}
  
  df[df$validity == FALSE , ] <- NA
  
  df$position <- position_temp
  
  if(!is.null(depth_top)){
    df$depth <- depth_temp
  }
} else if(na_validity==FALSE){
} else{
  stop('The na_validity parameter must be TRUE or FALSE.')
}

# return the values as logic
df$validity = as.logical(df$validity)

# make a pretty graph

if(!is.null(depth_top)){
  ylabel <- "Depth (mm)"
}else{ylabel <- "Position (mm)"}

if(graph==TRUE){
	require(analogue)
    if(!is.null(depth_top)){ Stratiplot(df[-grep("depth|position",colnames(df))], df$depth, varTypes = "absolute", 
                             absolutesize = "1", strip = TRUE, ylab=as.character(ylabel)) }
    else{	Stratiplot(df[-grep("depth|position",colnames(df))], df$position, varTypes = "absolute", 
  	      absolutesize = "1", strip = TRUE, ylab=as.character(ylabel)) }
} else if(graph==FALSE){
} else{
  stop('The graph parameter must be TRUE or FALSE.')
}

# export the data
if(!is.null(export)){
	write.table(df, file = export, sep = "\t")
} else{}

# return the data
return(df)
}

######################
## ITRAX-ORDINATION ##
######################

# dataframe     pass the name of a dataframe parsed using itrax_import
# elementsonly  TRUE or FALSE - false passes all data, TRUE only elements
# zeros         addone adds one to everything, limit replaces zeros with 0.001
# transform     perform centered log transform, or no transformation
# diagrams      draw some diagrams, or not

itrax_ordination=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE) {

# check the dataframe exists
if(is.data.frame(dataframe)){
	df <- dataframe
} else{
stop('Dataframe does not exist or object is not a dataframe.')
}

# rename the rows
if("depth" %in% colnames(df)) {
	rownames(df) <- round(df$depth)
	labels <- "Depth (mm)"
} else{
  df$position <- as.numeric(df$position)
	rownames(df) <- df$position
	labels <- "Position (mm)"
} 

# get rid of anything that isn't an element
elements = c("H", "He",
"Li", "Be", "B", "C", "N", "O", "F", "Ne",
"Na", "Mg", "Al", "Si", "P", "S", "Cl", 
# "Ar",
"K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
"Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
"Cs", "Ba", 
"La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", 
"Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn",
"Fr", "Ra", 
"Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", 
"Lr", "Unq", "Unp", "Unh", "Uns", "Uno", "Une", "Unn")

if(elementsonly==TRUE) {
	df <- df[ , which(names(df) %in% elements)]
} else if(elements==FALSE){
} else{
  stop('elements only must be true or false')
}

# deal with zero values
if(zeros=="addone") {
	df <- df + 1
} else if(zeros=="limit") {
	df[df == 0] <- 0.001
}else{
  warning('zero values may be present because zeros is not valid')
}  

# calculate centered log ratios for all elements in the original dataset
if(transform==TRUE) {
	require(chemometrics) # could also use "compositions" package
	df <- clr(df)
} else if(transform==FALSE) {
} else{
  stop('transform must be TRUE or FALSE')
}

# perform a principle components analysis of the original dataset and display the eigenvalues
df_pca <- prcomp(na.omit(df))
print(summary(df_pca$rotation))

# plot an ordination diagram of axes 1 and 2, with sample and variable scores, to explore the data
# then plot the first axis by position
# and also plot the axis one loadings
if(diagrams==TRUE) {
	dev.new()
  biplot(df_pca)
	dev.new()
	plot(row.names(df_pca$x), df_pca$x[ , 1], xlab=labels, ylab="PC1")
	dev.new()
	barplot(sort(df_pca$rotation[ , "PC1"]), horiz=TRUE, names.arg=row.names(sort(df_pca$rotation[ , "PC1"])), 
	        cex.names=0.6, xlab="PC1")
} else if(diagrams==FALSE){
} else{
  stop('diagrams must be TRUE or FALSE')
}

return(df_pca)
}

#######################
## ITRAX-CORRELATION ##
#######################

itrax_correlation=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE) {

# check the dataframe exists
if(is.data.frame(dataframe)){
  df <- dataframe
} else{
  stop('Dataframe does not exist or object is not a dataframe.')
}
  
# rename the rows
if("depth" %in% colnames(df)) {
  rownames(df) <- round(df$depth)
  labels <- "Depth (mm)"
} else{
  df$position <- as.numeric(df$position)
  rownames(df) <- df$position
  labels <- "Position (mm)"
} 
  
# get rid of anything that isn't an element
elements = c("H", "He",
             "Li", "Be", "B", "C", "N", "O", "F", "Ne",
             "Na", "Mg", "Al", "Si", "P", "S", "Cl", 
             # "Ar",
             "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
             "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
             "Cs", "Ba", 
             "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", 
             "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn",
             "Fr", "Ra", 
             "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", 
             "Lr", "Unq", "Unp", "Unh", "Uns", "Uno", "Une", "Unn")
  
if(elementsonly==TRUE) {
  df <- df[ , which(names(df) %in% elements)]
} else if(elementsonly==FALSE){
} else{
  stop('elementsonly must be true or false')
}
  
# deal with zero values
if(zeros=="addone") {
    df <- df + 1
} else if(zeros=="limit") {
    df[df == 0] <- 0.001
}else{
warning('zero values may be present because zeros is not valid')
}  
  
# calculate centered log ratios for all elements in the original dataset
if(transform==TRUE) {
  require(chemometrics) # could also use "compositions" package
  df <- clr(df)
} else if(transform==FALSE) {
} else{
    stop('transform must be TRUE or FALSE')
}

# run the correlation matrix
# in time, I'll add confidence levels to this
df_cor <- cor(df, use = "pairwise.complete.obs", method = "pearson")

# run diagrams
if(diagrams==TRUE) {
  require(corrplot)
  dev.new()
	corrplot.mixed(df_cor, lower="number", upper="color", order="AOE", number.cex=0.75 ) # and confidence levels to this
} else if(diagrams==FALSE){
} else{
  stop('diagrams must be TRUE or FALSE')
}

return(df_cor)
}


#######################
## ITRAX-AVERAGING   ##
#######################

itrax_averaging=function(dataframe, interval) {
  
# dataframe     pass the name of a dataframe parsed using itrax_import
# interval      pass the size of the interval in mm

# assert the dataframe exists and import it
if(is.data.frame(dataframe)){
  df <- dataframe
} else{
  stop('Dataframe does not exist or object is not a dataframe.')
}

# check if depths are present and subsistute position if not
if("depth" %in% colnames(df)) {
  position_subs <- FALSE
} else if("position" %in% colnames(df)){
  df$depth <- df$position
  position_subs <- TRUE
} else{
  stop('Neither depth nor columns are present. Did you parse you data in itrax_import?')
}

# work out the distance as row on row increment represents
scaninterval <- df[2, "depth"] - df[1, "depth"]
scaninterval <- round(scaninterval, digits=2)

# change ai to represent the equivalent number of rows
# round it so it is a whole number
interval <- interval / scaninterval
interval <- round(interval)

# perform the binning operation
df_avg          <- aggregate(df,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),mean)[-1]
df_avg$depthmin <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),min)$x
df_avg$depthmax <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),max)$x

# rename the columns if they need it
if(position_subs==TRUE){
  df_avg$position <- df_avg$depth
  df_avg$positionmin <- df_avg$depthmin
  df_avg$positionmax <- df_avg$depthmax
  df_avg$depth <- NULL
  df_avg$depthmin <- NULL
  df_avg$depthmax <- NULL
}else if(position_subs==FALSE){
}

return(df_avg)
}

#######################
##    ITRAX-JOIN     ##
#######################

# DEPTH MUST BE PRESENT
# list should be in the format "list(core1 = dataframe1, core 2 = dataframe2, etc...)"

# label the individual scans - this needs to iterate along the list called "list", e.g.
# dataframe1$labels <- deparse(substitute(dataframe1))
# dataframe2$labels <- deparse(substitute(dataframe2)) etc. etc.

itrax_join=function(list){
  
require(dplyr)

# label the data
list <- lapply(names(list), function(i) within(list[[i]], {label <- i}))

# join them
df <- bind_rows(list)

# sort them by depth
df <- df[with( df, order(depth) ) , ]

return(df)
}

#######################
##    ITRAX-META     ##
#######################

# Function takes a document.txt file and parses it into a sensible format.

itrax_meta=function(datafile="document.txt"){

# assert file exists
if (!file.exists(datafile)){
  stop('File doesn\'t exist, check your working directory.')
} else{}

# import the file
paths <- read.table(datafile, header = TRUE, sep = "\t", nrows = 1)
dates <- read.table(datafile, header = TRUE, sep = "\t", nrows = 1, skip = 2)
parameters <- read.table(datafile, header = FALSE, sep = "\t", skip = 4, stringsAsFactors = FALSE)

# housekeeping
parameters[ , 1] <- as.character(parameters[ , 1])
parameters[ , 3] <- as.character(parameters[ , 3])

# move those ones that arn't in the right place
stoprow <- c(parameters[8,3], as.numeric(parameters[8,4]), NA, NA)
names(stoprow) <- colnames(parameters)
xrfcurrent <- c("XRF current", parameters[9,4], NA, NA)
names(xrfcurrent) <- colnames(parameters)
opticalend <- c(parameters[11,3], parameters[11,4], NA, NA)
names(opticalend) <- colnames(parameters)
parameters <- rbind(parameters, stoprow, xrfcurrent, opticalend)

# re-label the columns
colnames(parameters) <- c("Parameter", "Value", "Unit")

# cleanup and rename some things
parameters[ , 4] <- NULL
parameters[6, 3] <- "ON/OFF"
parameters[9, 3] <- "kV"
parameters[9, 1] <- "XRF voltage"
parameters[10,3] <- "element"
parameters[8, 3] <- "mm"
parameters[19,3] <- "mm"
parameters[20,3] <- "mA"
parameters[21,3] <- "mm"
parameters[11,3] <- "mm"
parameters[1,1 ] <- "Rad. voltage"
parameters[2,1 ] <- "Rad. current"
parameters[3,1 ] <- "Rad. exposure"
parameters[12,1] <- "Optical step size"

# add the date
date <- paste( as.character(dates[1,3]),"/",as.character(dates[1,2]),"/",as.character(dates[1,1]), sep="" )
daterow <- c("Aquisition date", date, "dd/mm/yyyy")
names(daterow) <- colnames(parameters)
parameters <- rbind(parameters, daterow)

# add the names
samplename <- c("Sample name", as.character(paths[1,2]), "str")
names(samplename) <- colnames(parameters)
sectionname <- c("Section name", as.character(paths[1,3]), "str")
names(sectionname) <- colnames(parameters)
operatorname <- c("Operator name", as.character(paths[1,4]), "str")
names(operatorname) <- colnames(parameters)
  
parameters <- rbind(parameters, samplename, sectionname, operatorname)

# Sort it all into a sensible order
sortingorder <- c( "Sample name", "Section name", "Aquisition date", "Operator name",
                   "Tube", "Start coordinate", "Stop coordinate", "Step size", 
                   "Optical Start", "Optical End", "Optical step size",
                   "Rad. voltage", "Rad. current", "Rad. exposure", "line camera signal level", 
                   "XRF", "XRF voltage", "XRF current", "XRF exp. time", 
                   "Start temperature", "Stop temperature", "Start humidity", "Stop humidity", "Start vacuum", "Stop vacuum" )

parameters <- as.data.frame(parameters[match(sortingorder, parameters$Parameter),], stringsAsFactors = TRUE)
rownames(parameters) <- NULL

# pass the data
return(parameters)
}

#######################
##   ITRAX-SECTION   ##
#######################

itrax_section=function(dataframe, divisions=30, zeros="addone", elements=c(NULL), graph=TRUE){

# this function returns the following:
  # $samples - a list of suggested samples for analysis
  # $depth OR $position depending on what's present
  # $groups - the groups to go with $depth
  
# import the data  
# assert the dataframe exists and import it
  if(is.data.frame(dataframe)){
    df <- dataframe
} else{
    stop('Dataframe does not exist or object is not a dataframe.')
}
  
# rename the rows by depth 
if("depth" %in% colnames(df)) {
  rownames(df) <- round(df$depth)
  z_is <- "depth"
# or position
} else if("position" %in% colnames(df)) {
  rownames(df) <- round(df$position)
  z_is <- "position"
} else{
  stop('Neither depth nor position is present.')
}

# get rid of anything that isn't an element
element = c("H", "He",
             "Li", "Be", "B", "C", "N", "O", "F", "Ne",
             "Na", "Mg", "Al", "Si", "P", "S", "Cl", 
             # "Ar",
             "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
             "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
             "Cs", "Ba", 
             "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", 
             "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn",
             "Fr", "Ra", 
             "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", 
             "Lr", "Unq", "Unp", "Unh", "Uns", "Uno", "Une", "Unn")
# get rid of anything not in the list of elements above
df <- df[ , which(names(df) %in% element)]

# get rid of anything not in the specified list of elements
if(!is.null(elements)){
df <- df[ , which(names(df) %in% elements)]
}else{}
  
# deal with zero values
if(zeros=="addone") {
  df <- df + 1
} else if(zeros=="limit") {
  df[df == 0] <- 0.001
} else{stop('zeros must be addone or limit')} 

# deal with NA values
df <- na.omit(df)
  
# centered log ratio transform the data
require(chemometrics) # could also use "compositions" package
df <- clr(df)

# perform a cluster analysis using Euclidian distances
d <- dist(as.matrix(df))
hc <- hclust(d, method = "ward.D2")

# draw a dendrogram
if(graph==TRUE){
plot(hc)
dev.new()
} else if(graph==FALSE){
} else{stop('graph must be true or false')}

# divide into groups
groups <- cutree(hc, k=divisions)
df$group <- groups

# do a cluster analysis of each group individually
sample_list <- sample_list <- c(1:as.numeric(divisions))
# sample_list <- NA
loop_group <- 1

while( loop_group < divisions ){
  
  loop_d <- dist( as.matrix(df[ df$group == loop_group , ]) )
  loop_hc <- hclust( loop_d, method="ward.D2" )
  
  # convert the index number to a depth label
  loop_reorder <- data.frame(loop_hc$labels, loop_hc$order)
  loop_reorder <- loop_reorder[order(loop_reorder$loop_hc.order),]

  # pick the MIDDLE in each group
  loop_centerpoint <- round(length(loop_reorder$loop_hc.labels) / 2)
  
  loop_center <- as.character(loop_reorder[loop_centerpoint,1])
  
 
  # append the depth or position of each of those samples to a list
  sample_list[loop_group] <- loop_center
  #sample_list <- c(sample_list, loop_center)
  
  loop_group <- loop_group + 1
}

# sort the list of samples numerically for the sake of neatness
sample_list <- sort(as.numeric(sample_list))

# print the list for diagnostics
print(sample_list)

# draw a picture
if(graph==TRUE){
require(ggplot2)
p <- ggplot(df, aes(row.names(df), fill=as.factor(groups))) + geom_bar()
print(p)
} else if(graph==FALSE){
} else{stop('graph must be true or false')}

# make a small table with the position OR depth, and put the groups there
group_table <- as.data.frame(df$group)
row.names(group_table) <- row.names(df)
colnames(group_table) <- "group"

if(z_is=="depth"){
  return(list( "groups"=c(group_table$group), "depth"=c(row.names(df)), "samples"=c(sample_list) ) )
} else if(z_is=="position"){
  return(list( "groups"=c(group_table$group), "position"=c(row.names(df)), "samples"=c(sample_list) ) )  
}

}

##########################
## ITRAX-SPECTRACOMPARE ##
##########################

itrax_spectracompare=function(filea, fileb, datapos=30, graph=TRUE) {
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
    plot(dfa[ , 3], dfa[ , 2], type="l", col="blue", log="y", xlab = "Energy (kEV)", ylab = "Counts")
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