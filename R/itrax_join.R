#' Join two or more Itrax result datasets
#'
#' Join two or more Itrax datasets that have been parsed using \code{"itrax_import()"}
#'
#' @param list a list of dataframes that are parsed Itrax result files --- this should have been imported using \code{"itrax_import()"} and must have a depth variable present. This should take the form \code{"list(core1 = core1, core2 = core2)"}
#'
#' @return a tibble of all the input data
#'
#' @export
#'
#' @examples
#' itrax_join(list(core1 = CD166_19_S1$xrf, core2 = CD166_19_S1$xrf))

itrax_join <- function(list){
  if(all(unlist(lapply(list, function(x) "depth" %in% colnames(x)))) == FALSE){
    stop("Depth variable is required when joining core sections, but is missing from one or more sections.")
  } else{
    df <- lapply(names(list), function(i) within(list[[i]], {label <- i})) %>%
      bind_rows() %>%
      arrange(depth)
  }

  return(df)
}
