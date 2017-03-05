#' Join two or more Itrax result datasets
#'
#' Join two or more Itrax datasets that have been parsed using \code{"itrax_import()"}
#'
#' @param list a list of dataframes that are parsed Itrax result files --- this should have been imported using \code{"itrax_import()"} and must have a depth variable present
#' #'
#' @return a dataframe of all the input data
#'
#' @examples
#' itrax_join( list(core1 = df1, core2 = df2, core3 = df3) )
#'
#' @export

# DEPTH MUST BE PRESENT
# list should be in the format "list(core1 = dataframe1, core 2 = dataframe2, etc...)"

# label the individual scans - this needs to iterate along the list called "list", e.g.
# dataframe1$labels <- deparse(substitute(dataframe1))
# dataframe2$labels <- deparse(substitute(dataframe2)) etc. etc.

itrax_join=function(list){

  #require(dplyr)

  # label the data
  list <- dplyr::lapply(names(list), function(i) within(list[[i]], {label <- i}))

  # join them
  df <- dplyr::bind_rows(list)

  # sort them by depth
  df <- df[with( df, order(depth) ) , ]

  return(df)
}
