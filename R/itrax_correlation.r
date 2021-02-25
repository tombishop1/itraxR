#' Calculate a correlation matrix for Itrax result data
#'
#' Calculates a correlation matrix for Itrax data results including normalisation and visualisation
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly if TRUE, only chemical elements are included. If FALSE, the data is passed unfiltered, otherwise a character vector of desired variable names can be supplied
#' @param zeros if "addone", adds one to all values. If "limit", replaces zero values with 0.001. Otherwise a function can be supplied to remove zero values.
#' @param transform binary operator that if TRUE will center-log-transform the data, if FALSE will leave the data untransformed. Otherwise, a function can be supplied to transform the data.
#' @param plot set to true if a biplot is required as a side-effect
#'
#' @return a correlation matrix object
#'
#' @examples
#' itrax_correlation(CD166_19_S1$xrf, plot = TRUE)
#'
#' @importFrom stats cor na.omit
#' @importFrom utils data
#' @importFrom ggcorrplot ggcorrplot cor_pmat
#' @export

itrax_correlation = function(dataframe,
                             elementsonly = TRUE,
                             zeros = "addone",
                             transform = TRUE,
                             plot = FALSE){

  # fudge to stop check notes
  . = NULL
  group = NULL
  ids = NULL
  position = NULL

  # label with ids
  dataframe$ids <- 1:dim(dataframe)[1]
  input_dataframe <- dataframe

  # use internal function to do multivariate data preparation
  dataframe <- multivariate_import(dataframe = dataframe,
                                   elementsonly = TRUE,
                                   zeros = "addone",
                                   transform = TRUE)

  # run the correlation matrix
  # in time, I'll add confidence levels to this
  cor_matrix <- cor(dataframe,
                use = "pairwise.complete.obs",
                method = "pearson")

  # draw a summary diagram if required
  # make a plot if required
  if(is.logical(plot) == TRUE && plot == TRUE){
    print(ggcorrplot::ggcorrplot(cor_matrix,
                                 hc.order = TRUE,
                                 p.mat = cor_pmat(cor_matrix, method = "spearman"),
                                 insig = "blank"))
  } else if(is.logical(plot) == FALSE){
    stop("plot parameter must be logical (TRUE/FALSE)")
  }

  # deal with the returns
  return(cor_matrix)
}

