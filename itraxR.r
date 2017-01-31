# itraxR --- Functions to manipulate Itrax data files

###################
## ITRAX-IMPORT  ##
###################

itrax_import=function(datafile="result.txt", depth_top=NULL, trim_top=NULL, trim_bottom=NULL, na_validity=NULL, few_parameters=TRUE, graph=NULL, export=NULL) {
# datafile			= 	name of the input data
# depth_top			= 	defines the coring depth of the top of the scan
# trim_top			= 	how much bad data at the start of the scan to remove
# trim_bottom		= 	how much bad data at the end of the scan to remove
# na_validity		= 	makes invalid data "NA" - on by default
# few_parameters	= 	TRUE (default) passes elemental and limited metadata
#						FALSE passes all data without removing anything
# graph				=	draws a graph
# export			=	exports the data

# assert file exists
if (!file.exists(datafile)){
  stop('File doesn\'t exist, check your working directory.')
  return()
}

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
if(few_parameters==TRUE){
	df <- df[ , which(names(df) %in% append(elements, others))]
} else if(few_parameters==FALSE){
	df <- df
# } else if(few_parameters=="ELEMENTS"){
#	df <- df[ , which(names(df) %in% elements)]
#} else {
#	df <- df[ , which(names(df) %in% few_parameters)]
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
}

# Replace all rows with validity == 0 with NA
if(!is.null(na_validity)){
	df[df$validity == FALSE , ] <- NA
	df$validity[is.na(df$validity)] <- 0 
}

# return the values as logic
df$validity = as.logical(df$validity)

# trim the junk data at the beginning of the scan
if(!is.null(trim_top)){
	top_trim_position <- min(df$depth) + trim_top
	df <- subset(df, df$depth > top_trim_position)
} 

# trim the junk data at the end of the scan
if(!is.null(trim_bottom)){
	bottom_trim_position <- max(df$depth) - trim_bottom 
	df <- subset(df, df$depth < bottom_trim_position)
}

# make a pretty graph
if(!is.null(graph)){
	require(analogue)
	Stratiplot(df[-grep("depth|position",colnames(df))], df$depth, varTypes = "absolute", absolutesize = "1", strip = TRUE)
}

# export the data
if(!is.null(export))
	write.table(df, file = export, sep = "\t")

# return the data
return(df)
}

######################
## ITRAX-ORDINATION ##
######################

itrax_ordination=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE) {

# check the dataframe exists
if(is.data.frame(dataframe)){
# if(exists(dataframe) && is.data.frame(get(dataframe))){
	df <- dataframe
} #else {
#	stop('Dataframe does not exist or object is not a dataframe.')
#	return()
#}

# rename the rows
#if("depth" %in% colnames(df)) {
#	rownames(df) <- round(df$depth)
#} else if("position" %in% colnames(df)) {
#	rownames(df) <- round(df$position)
#} 

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
} else{
}

# deal with zero values
if(zeros=="addone") {
	df <- df + 1
} else if(zeros=="limit") {
	df[df == 0] <- 0.001
}  

# calculate centered log ratios for all elements in the original dataset
if(transform==TRUE) {
	require(chemometrics) # could also use "compositions" package
	df <- clr(df)
}

# perform a principle components analysis of the original dataset and display the eigenvalues
df_pca <- prcomp(df)
print(df_pca$rotation)

# plot an ordination diagram of axes 1 and 2, with sample and variable scores, to explore the data
# then plot the first axis by position
# and also plot the axis one loadings
if(diagrams==TRUE) {
	biplot(df_pca)
	dev.new()
	plot(row.names(df_pca$x), df_pca$x[ , 1])
	dev.new()
	barplot(sort(df_pca$rotation[ , "PC1"]), horiz=TRUE, names.arg=row.names(sort(df_pca$rotation[ , "PC1"])), cex.names=0.6)
}

return(df_pca)
}

#######################
## ITRAX-CORRELATION ##
#######################

itrax_correlation=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE) {
require(corrplot)
# check the dataframe exists
if(is.data.frame(dataframe)){
 #if(exists(dataframe) && is.data.frame(get(dataframe))){
	df <- dataframe
} #else {
#	stop('Dataframe does not exist or object is not a dataframe.')
#	return()
#}

# rename the rows
#if("depth" %in% colnames(df)) {
#	rownames(df) <- round(df$depth)
#} else if("position" %in% colnames(df)) {
#	rownames(df) <- round(df$position)
#} 

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
} else{
}

# deal with zero values
if(zeros=="addone") {
	df <- df + 1
} else if(zeros=="limit") {
	df[df == 0] <- 0.001
}  

# calculate centered log ratios for all elements in the original dataset
if(transform==TRUE) {
	require(chemometrics) # could also use "compositions" package
	df <- clr(df)
}

# run the correlation matrix
df_cor <- cor(df, use = "pairwise.complete.obs", method = "pearson")

# run diagrams
if(diagrams==TRUE) {
	dev.new()
	corrplot.mixed(df_cor, lower="number", upper="color", order="AOE" )
}

return(df_cor)
}


#######################
## ITRAX-AVERAGING   ##
#######################

itrax_averaging=function(dataframe, interval) {

df <- dataframe

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

return(df_avg)
}

#######################
##    ITRAX-JOIN     ##
#######################

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

itrax_section=function(dataframe, divisions=30, zeros="addone", elements=c( "Na", "Al", "Si", "Ca", "Ti", "Mn", "Fe", "Pb" )){
# import the data  
df <- dataframe
  
# rename the rows by depth 
if("depth" %in% colnames(df)) {
  rownames(df) <- round(df$depth)
# or position
} else if("position" %in% colnames(df)) {
  rownames(df) <- round(df$position)
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
df <- df[ , which(names(df) %in% elements)]
  
# deal with zero values
if(zeros=="addone") {
  df <- df + 1
} else if(zeros=="limit") {
  df[df == 0] <- 0.001
}  
  
# centered log ratio transform the data
require(chemometrics) # could also use "compositions" package
df <- clr(df)

# perform a cluster analysis using Euclidian distances
d <- dist(as.matrix(df))
hc <- hclust(d, method = "ward.D2")

# draw a dendrogram
plot(hc)
dev.new()

# divide into groups
groups <- cutree(hc, k=divisions)
df$group <- as.data.frame(groups)

# work out the largest contiguous group in each class

# report the median depth of each of those largest contiguous groups

# draw a picture
require(ggplot2)
p <- ggplot(df, aes(row.names(df), fill=as.factor(groups))) + geom_bar()
print(p)

# return a diagram showing the different clusters as a function of depth
return(hc)
}