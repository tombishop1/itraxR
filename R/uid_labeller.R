#' Generate uids for datasets
#'
#' Assigns unique identifiers to dataset observations. 
#' If there is already a column called uid, that does infact contain unique ids, it simply uses them. 
#' Otherwise, it looks for the columns `depth` and `label`, and generates a meaningful identifier by combining them.
#' Otherwise, it simply uses consecutive numbers prefixed by `uid`. 
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' 
#' @importFrom rlang .data
#' 
#' @keywords internal

uid_labeller <- function(dataframe){
  if("uid" %in% colnames(dataframe) && !TRUE %in% dataframe$uid %>% duplicated(.data)){
    dataframe$uid <- as.character(dataframe$uid)
  } else if("label" %in% colnames(dataframe) && 
            "depth" %in% colnames(dataframe) && 
            !TRUE %in% paste0(dataframe$label, "_", dataframe$depth) %>% duplicated(.data)){
    dataframe$uid <- paste0(dataframe$label, "_", dataframe$depth)
  } else{
    dataframe$uid <- paste0("uid", 1:dim(dataframe)[1])
  }
  return(dataframe)
}
