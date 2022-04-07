#' Import Itrax core-scanner result file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param filename defines the name of the datafile to parse
#' @param depth_top defines the coring in depth of the top of the core, in mm
#' @param trim_top defines the length of any trimming required of data at the top of the core, in mm
#' @param trim_bottom defines the length of any trimming required at the bottom of the core, in mm
#' @param parameters one of `all` (leave all parameters), `some` (remove some less useful parameters)
#'
#' @return a tibble of the parsed Itrax data
#'
#' @examples
#' itrax_import(
#'   filename = system.file("extdata",
#'                          "CD166_19_S1_Results.txt",
#'                          package = "itraxR",
#'                          mustWork = TRUE),
#'   depth_top = 0)
#'
#' @import dplyr

#' @importFrom rlang .data
#' @importFrom readr read_tsv
#' @importFrom janitor remove_empty
#'
#' @export

itrax_import <- function(filename = "Results.txt", depth_top = NA, trim_top = 0, trim_bottom = 0, parameters = "some"){
  
  elements <- periodicTable$symb
  others <- c( "depth", "position (mm)", "sample.surface", "MSE", "cps", "kcps", "validity", "Mo inc", "Mo coh", "Cr inc", "Cr coh" )
  
  . <- NULL
  position <- NULL
  validity <- NULL
  `position (mm)` <- NULL
  
  # report errors
  if(!file.exists(filename)){
    stop("That filename doesn't exist, check your working directory.")
  }
  
  #if(!is.numeric(depth_top) | !is.na(depth_top)){
  #  stop("The specified top depth must be NA or numeric.")
  #}
  
  if(!is.numeric(trim_top) | !is.numeric(trim_bottom)){
    stop("Trims must be numeric")
  }
  
  if(!parameters %in% c("some", "all")){
    warning("The parameters should be `some` or `all` - other values might cause unexpected behaviour.")
  }
  
  # import and tidy
  df <- suppressMessages(suppressWarnings(readr::read_tsv(filename, skip = 2))) %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    mutate(validity = as.logical(validity)) %>%
    # filter if requested
    {if(parameters == "some") 
      select(., any_of(c(elements, others))) else .} %>%
    rename(position = `position (mm)`) %>%
    # sort out position and depth
    {if(is.numeric(depth_top) == TRUE)      
      mutate(., depth = position - min(position) + depth_top) else .} %>%
    # sort out those zero values where valididy == 0
    mutate(across(any_of(elements), function(x){if_else(validity == TRUE, 
                                             true = x,
                                             false = na_if(x, 0))}
                  )
           ) %>%
    # trim top and bottom if required 
    filter(position >= min(position) + trim_top & position <= max(position) - trim_bottom) %>%
    # tidy up
    select(any_of(others[1:7]), any_of(elements), any_of(others[8:length(others)]), everything())
  
  # return 
  return(df)
}
