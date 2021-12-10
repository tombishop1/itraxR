#' Prepare data for multivariate analysis
#'
#' does all the preparation work for multivariate methods
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly if TRUE, only chemical elements are included. If FALSE, the data is passed unfiltered, otherwise a character vector of desired variable names can be supplied
#' @param zeros if "addone", adds one to all values. If "limit", replaces zero values with 0.001. Otherwise a function can be supplied to remove zero values.
#' @param transform binary operator that if TRUE will center-log-transform the data, if FALSE will leave the data untransformed. Otherwise, a function can be supplied to transform the data.
#'
#' @importFrom tidyr drop_na
#' @importFrom compositions clr
#' @importFrom rlang .data
#'
#' @keywords internal

multivariate_import <- function(dataframe,
                                elementsonly,
                                zeros,
                                transform){

  # check if uid exists, error if not
  if("uid" %in% colnames(dataframe)){ 
    dataframe <- dataframe %>%
      tibble::column_to_rownames(var = "uid")
  } else(
    stop("you must pass a `uid` column to `multivariate_import()` - use `uid_labeller()`.")
  )

  # trim to only the elements
  if(is.logical(elementsonly) == TRUE && elementsonly==TRUE){
    dataframe <- dataframe %>%
      select(any_of(periodicTable$symb))
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))
  } else if(is.logical(elementsonly) == TRUE && elementsonly==FALSE){
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))
  } else{
    dataframe <- dataframe %>%
      select(any_of(elementsonly))
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))}

  # deal with the zeros
  if(zeros=="addone"){
    dataframe <- dataframe + 1
    dataframe <- dataframe %>% tidyr::drop_na()
  } else if(zeros=="limit"){
    dataframe <- dataframe %>%
      mutate(across(any_of(periodicTable$symb), ~recode(.data, `0` = 0.001))) %>%
      tidyr::drop_na()
  } else{
    dataframe <- dataframe %>%
      mutate(across(any_of(periodicTable$symb), zeros)) %>%
      tidyr::drop_na()
  }
  
  # deal with the transformation
  if(is.logical(transform) == TRUE && transform==TRUE){
    dataframe <- dataframe %>%
      mutate(across(everything(), function(x){ifelse(x == 0, -1, x)})) %>%
      compositions::acomp()
  } else if(is.logical(transform) == TRUE && transform==FALSE){
    dataframe <- dataframe
  } else{
    dataframe <- transform(dataframe)
  }

  # returns
  return(dataframe)
}
