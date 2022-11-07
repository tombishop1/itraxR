#' Principle Component Analysis on Itrax scan data
#'
#' Performs and visualises principle component analysis data from Itrax result data
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly if TRUE, only chemical elements are included. If FALSE, the data is passed unfiltered, otherwise a character vector of desired variable names can be supplied
#' @param zeros if "addone", adds one to all values. If "limit", replaces zero values with 0.001. Otherwise a function can be supplied to remove zero values.
#' @param transform binary operator that if TRUE will center-log-transform the data, if FALSE will leave the data untransformed. Otherwise, a function can be supplied to transform the data.
#' @param return if "pca" the output of \code{prcomp()} is returned, otherwise "list" is a list including the transformed data, sample scores, and the output of prcomp().
#' @param plot set to true if a biplot is required as a side-effect
#'
#' @importFrom stats biplot
#' @importFrom rlang .data
#' @importFrom broom augment
#' @importFrom tibble rownames_to_column
#' @import compositions
#'
#' @return either an output of \code{prcomp()}, or a list including the input data
#'
#' @examples
#' itrax_ordination(CD166_19_S1$xrf, plot = TRUE)
#'
#' @export

itrax_ordination <- function(dataframe,
                             elementsonly = TRUE,
                             zeros = "addone",
                             transform = TRUE,
                             return = "list",
                             plot = FALSE){

  .Deprecated(new = "compositions::princomp.acomp",
              msg = "`itraxR::itrax_ordination` is deprecated. \n Call compositions::princomp.acomp() directly instead.")

  # label with ids
  dataframe <- dataframe %>%
    uid_labeller()

  # use internal function to do multivariate data preparation
  input_dataframe <- multivariate_import(dataframe = dataframe,
                                   elementsonly = elementsonly,
                                   zeros = zeros,
                                   transform = transform)
  
  # perform the PCA
  pca <- input_dataframe %>%
    compositions::zeroreplace() %>%
   compositions::princomp.acomp()

  # make a plot if required
  if(plot == TRUE){
    stats::biplot(pca, xlabs = rep(".",times = nrow(input_dataframe)))
  }


  # select the return
  if(return == "pca"){
    return(pca)
    } else if(return == "list"){
      scores <- as_tibble(pca$scores, rownames = NA) %>%
        tibble::rownames_to_column("uid")
      output_dataframe <- left_join(dataframe, scores, by = "uid")
      return(list(pca = pca,
                  data = output_dataframe))
      }
}
