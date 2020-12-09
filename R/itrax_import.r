#' Import Itrax core-scanner result file
#'
#' Imports and parses data from a results file created by Q-Spec software, part of the Itrax core scanner.
#'
#' @param datafile defines the name of the datafile to parse
#' @param depth_top defines the coring in depth of the top of the core, in mm
#' @param trim_top defines the length of any trimming required of data at the top of the core, in mm
#' @param trim_bottom defines the length of any trimming required at the bottom of the core, in mm
#' @param parameters one of all (leave all parameters), some (remove some less useful parameters)
#'
#' @return a tibble of the parsed Itrax data
#'
#' @export


itrax_import <- function(filename = "Results.txt", depth_top = NA, trim_top = 0, trim_bottom = 0, parameters = "some"){

  # create elements list
  data("periodicTable", package = "PeriodicTable")
  elements <- periodicTable$symb

  others <- c( "position (mm)", "sample.surface", "MSE", "cps", "validity", "Mo inc", "Mo coh", "Cr inc", "Cr coh" )

  # import and tidy
  df <- suppressMessages(suppressWarnings(read_delim::read_tsv(filename, skip = 2))) %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    mutate(validity = as.logical(validity))

  # remove stuff we don't need
  if(parameters == "some"){
    df <- df %>% select(any_of(c(elements, others)))
  } else if(parameters == "all"){
  } else{stop("parameters must be some or all.")}

  # sort out the position and depth
  if(is.numeric(depth_top) == TRUE){
    df <- df %>% rename(position = `position (mm)`) %>%
      mutate(depth = position - min(position) + depth_top) %>%
      select(depth, everything())
  } else if(!is.na(depth_top)){
    stop("depth_top must be numeric or NA.")
  } else{df <- df %>% rename(position = `position (mm)`)}

  # sort out bad data
  if(is.numeric(depth_top) == TRUE){
    df <- suppressMessages(full_join(df %>% filter(validity == TRUE),
                                     df %>% filter(validity == FALSE) %>% select(any_of(c(elements, "position"))) %>% na_if(0))) %>%
      arrange(position) %>%
      mutate(depth = df$depth,
             validity = df$validity,
             cps = df$cps,
             MSE = df$MSE) %>%
      select(any_of("depth"), any_of(others[1:5]), any_of(elements) , any_of(others[6:length(others)]), everything())
  } else{
    df <- suppressMessages(full_join(df %>% filter(validity == TRUE),
                                     df %>% filter(validity == FALSE) %>% select(any_of(c(elements, "position"))) %>% na_if(0))) %>%
      arrange(position) %>%
      mutate(validity = df$validity,
             cps = df$cps,
             MSE = df$MSE) %>%
      select(any_of(others[1:5]), any_of(elements) , any_of(others[6:length(others)]), everything())
  }
  # cut the ends
  if((is.numeric(trim_top) && is.numeric(trim_bottom)) == TRUE){
    df <- df %>% filter(position > min(position) + trim_top & position < max(position) - trim_bottom)
  } else{
    stop("trim_bottom and trim_top should be numeric - set to 0 if no trim required")
  }

  # tidy up
  return(df)
}
