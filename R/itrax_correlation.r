#' Calculate a correlation matrix for Itrax result data
#'
#' Calculates a correlation matrix for Itrax data results including normalisation and visualisation
#'
#' @param dataframe defines the name of the dataframe to reduce --- this should have been imported using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly logical operator that if TRUE will only perform the analysis for elemental data
#' @param zeros can be either "addone" or "limit" --- this defines what to do with zero values when normalisating. Limit uses 0.001 as the zero value, add one adds one to all data
#' @param transform logical operator that if TRUE will center-log-transform the data
#' @param diagrams logical operator, if TRUE will produce a graphical output
#'
#' @return a correlation matrix object
#'
#' @examples
#' \dontrun{itrax_correlation(df)}
#'
#' @export

itrax_correlation=function(dataframe, elementsonly=TRUE, zeros="addone", transform=TRUE, diagrams=TRUE) {

  # check the dataframe exists
  if(is.data.frame(dataframe)){
    df <- dataframe
  } else{
    stop('Dataframe does not exist or object is not a dataframe.')
  }

  # rename the rows
  # if("depth" %in% colnames(df)) {
  #   rownames(df) <- round(df$depth)
  #   labels <- "Depth (mm)"
  # } else{
  #   df$position <- as.numeric(df$position)
  #   rownames(df) <- df$position
  #   labels <- "Position (mm)"
  # }
  rownames(df) <- seq(from = 1, length.out = dim(df)[1])

  # get rid of anything that isn't an element
  require(PeriodicTable)
  data("periodicTable", package = "PeriodicTable")
  elements <- periodicTable$symb

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
#    require(chemometrics) # could also use "compositions" package
#    df <- clr(df)
     df <- chemometrics::clr(df)
  } else if(transform==FALSE) {
  } else{
    stop('transform must be TRUE or FALSE')
  }

  # run the correlation matrix
  # in time, I'll add confidence levels to this
  df_cor <- cor(df, use = "pairwise.complete.obs", method = "pearson")

  # run diagrams
  if(diagrams==TRUE) {
    #require(corrplot)
    #corrplot.mixed(df_cor, lower="number", upper="color", order="AOE", number.cex=0.75 ) # and confidence levels to this
    corrplot::corrplot.mixed(df_cor, lower="number", upper="color", order="FPC", number.cex=0.75 ) # and confidence levels to this
  } else if(diagrams==FALSE){
  } else{
    stop('diagrams must be TRUE or FALSE')
  }

  return(df_cor)
}

