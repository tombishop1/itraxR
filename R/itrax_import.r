#' Import Itrax core-scanner result file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param datafile defines the name of the datafile to parse
#' @param depth_top defines the coring in depth of the top of the core, in mm
#' @param trim_top defines the length of any trimming required of data at the top of the core, in mm
#' @param trim_bottom defines the length of any trimming required at the bottom of the core, in mm
#' @param na_validity binary operator that defines whether to set all invalid data to NA
#' @param parameters one of ALL (leave all parameters), SOME (remove some less useful parameters), or ELEMENTS to leave only elemental data
#' @param graph binary operator that defines whether a graphical output should be produced
#' @param export defines the name of a text file if an output file should be produced
#'
#' @return a dataframe of the parsed Itrax data
#'
#' @examples
#' itrax_import(datafile=system.file("extdata", "result.txt", package = "itraxR"), depth_top=0, trim_top=5, trim_bottom=5, graph=FALSE)
#'
#' @export

# BUG! If the trims ae different, there is a fatal error

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
  others = c("position..mm.", "sample.surface", "MSE", "cps", "validity", "Mo.inc", "Mo.coh", "Cr.inc", "Cr.coh")

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
    if(!is.null(depth_top)){ analogue::Stratiplot(df[-grep("depth|position",colnames(df))], df$depth, varTypes = "absolute",
                                        absolutesize = "1", strip = TRUE, ylab=as.character(ylabel)) }
    else{	analogue::Stratiplot(df[-grep("depth|position",colnames(df))], df$position, varTypes = "absolute",
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
