#' Principle Component Analysis on Itrax scan data
#'
#' Performs and visualises principle component analysis data from Itrax result data
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly logical operator that if TRUE will only perform the analysis for elemental data
#' @param zeros can be either "addone" or "limit" --- this defines what to do with zero values when normalisating. Limit uses 0.001 as the zero value, add one adds one to all data
#' @param transform logical operator that if TRUE will center-log-transform the data
#' @param diagrams logical operator, if TRUE will produce a graphical output
#'
#' @return an ordination results object
#'
#' @examples
#' itrax_ordination(df)
#'
#' @export

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
