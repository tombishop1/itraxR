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
#' @importFrom stats prcomp
#' @importFrom rlang .data
#' @importFrom broom augment
#' @import ggfortify
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

  # fudge to stop build notes
  ids <- NULL

  # label with ids
  dataframe$ids <- 1:dim(dataframe)[1]
  input_dataframe <- dataframe

  # use internal function to do multivariate data preparation
  dataframe <- multivariate_import(dataframe = dataframe,
                                   elementsonly = TRUE,
                                   zeros = "addone",
                                   transform = TRUE)

  # perform the PCA
  pca <- dataframe %>%
    scale() %>%
    prcomp()

  # make a plot if required
  if(is.logical(plot) == TRUE && plot == TRUE){
  print(autoplot(pca,
           loadings = TRUE,
           loadings.label = TRUE))
  } else if(is.logical(plot) == FALSE){
    stop("plot parameter must be logical (TRUE/FALSE)")
  }

  # select the return
  if(return == "pca"){
    #warning("`return = \"pca\"` will be depreciated, but for now is retained for compatibility.") # not sure about this yet
    return(pca)
    } else if(return == "list")

      transformed_data <- pca %>%
        broom::augment(as_tibble(dataframe)) %>%
        select(any_of(c(names(dataframe), ".fittedPC1", ".fittedPC2", ".fittedPC3")))

      return(list(pca = pca,
                  data = left_join(as_tibble(input_dataframe) %>%
                                           select(c(names(input_dataframe)[!names(input_dataframe) %in% names(as_tibble(transformed_data))], "ids")),
                                   as_tibble(transformed_data),
                                         by = "ids") %>%
                    select(names(input_dataframe), everything()) %>%
                    select(-ids)
                  )
             )
}
