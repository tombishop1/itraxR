# Function to read in Itrax result files. 
# The result file will usually be named result.txt or results.txt

# Usage:
  # Assign loaded data like this:
  # itrax_data=itrax_import('result.txt',33)

itrax_import=function(datafile,depth_top){
# assert file exists
if (!file.exists(datafile)){
  stop('File doesn\'t exist')

return(list=datafile,depth_top)
}

# read the result file into a dataframe
df = read.table(datafile, skip = 2, header = TRUE, sep = "\t")

# remove variables of no interest - this could be modified to only include specified variables using "df[grep(..."
df = df[-grep("datafile|E.gain|E.offset|F.slope|F.offset|X|Fe.a.2|Fe.a.b|W.La|W.Lb1|W.lb2|W.La.in|W.Lb1.in|W.Lb2.in|Dt",colnames(df))]

# re-name the position variable
colnames(df)[which(names(df) == "position..mm.")] <- "position"

# convert the binary values in validity to true/false
df$validity = as.logical(df$validity)

# convert position into an numeric value
df$position = as.numeric(df$position)

# display some information about what just got loaded in
message("Loaded ", nrow(df), " rows of data")

# calculate the new depth variable
depth_offset = depth_top - df[1, "position"]
df$depth = df$position + depth_offset

# make a pretty graph # glc. Unsure why this doesn't work inside the function but does outside.
# require(analogue)
# Stratiplot(df[-grep("depth|position",colnames(df))], df$depth, varTypes = "absolute", absolutesize = "1", strip = TRUE)

return(df)
}
