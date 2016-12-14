# itraxR
#
# Functions to manipulate Itrax data files
# The following packages are required: analogue, chemometrics, Hmisc
#

###################
## ITRAX-IMPORT  ##
###################

itrax_import=function(datafile="result.txt", depth_top=NULL, trim_top=NULL, trim_bottom=NULL, na_validity=NULL, few_parameters=TRUE, graph=NULL, export=NULL){

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
} else if(few_parameters==ELEMENTS){
	df <- df[ , which(names(df) %in% elements)]
} else {
	df <- df[ , which(names(df) %in% few_parameters)]
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
	df <- subset(df, df$depth > trim_top)
} 

# trim the junk data at the end of the scan
if(!is.null(trim_bottom)){
	df <- subset(df, df$depth < trim_bottom)
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

itrax_ordination=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE){

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

######################
## ITRAX-ORDINATION ##
######################

itrax_correlation=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE){

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

# run pairs
if(diagrams=TRUE) {
	dev.new()
	pairs(df)
}

# now run the correlation matrix
require(Hmisc)
df <- rcorr(as.matrix(df), type = "pearson")

return(df)
}
