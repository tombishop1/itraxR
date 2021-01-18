#' Reduce Itrax XRF data
#'
#' Reduces Itrax XRF data into arbitrary chunks using an arbitrary function.
#' This is useful when making direct comparisons between the Itrax XRF data and some other data collected at a lower resolution.
#'
#' @param dataframe defines the name of the XRF data to reduce, usually a itraxR::itrax_import like tibble
#' @param breaks_lower a vector of the lower limit of each chunk
#' @param breaks_upper a vector of the upper limit of each chunk
#' @param fun the function to apply in order to reduce the data. Default is mean(), but sd() is also common
#' @param edges a vector of length 2 with the upper and lower bound behaviour; can be any of `<`, `<=`, `>`, `>=`
#' @param by if contiguous samples of even sizes are required, `by` defines the chunk size and will automatically generate `breaks`
#' @param names optional, a vector of the same length as `breaks`
#'
#' @return a tibble with the same number of rows as `breaks` and the same number of columns as `dataframe`
#'
#' @importFrom rlang .data
#'
#' @import dplyr
#'
#' @export

itrax_reduce <- function(dataframe,
                         names = c(1:length(breaks_lower)),
                         breaks_lower,
                         breaks_upper,
                         fun = mean,
                         edges = c(">=", "<"),
                         by = NULL){

  # provides for `intervals` parameter for an easy life
  if(is.null(by) == FALSE){
    breaks_lower = seq(from =  min(dataframe$depth), to = max(dataframe$depth), by = by)
    breaks_upper = breaks_lower + by
    names        = seq(from = 1, to = length(breaks_lower), by = 1)
  }

  # convert depth ranges to a list
  classMatrix <- outer(dataframe$depth, breaks_lower, edges[1]) & outer(dataframe$depth, breaks_upper, edges[2])

  # check if there are overlaps
  if(all(!rowSums(classMatrix) > 1) == FALSE){
    message(paste("Some of the reduced sample ranges overlap.",
                  "Did you really take overlapping sub-samples? Check the behaviour of the `edges` parameter.",
                  sep = "\n"))}

  # convert that matrix to a list
  classList <- split(t(classMatrix), seq(nrow(t(classMatrix))))

  # make the calculations
  calcList <- sapply(classList,
                     FUN = function(x) dataframe[x,] %>% sapply(fun),
                     simplify = TRUE) %>%
    t() %>%
    as_tibble() %>%
    mutate(resample_names = names) %>%
    select(.data$resample_names, everything())
  return(calcList)
}
