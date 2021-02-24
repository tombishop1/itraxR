#' Cluster analysis and statistical grouping of Itrax data
#'
#' Performs a cluster analysis and automatic statistical grouping of parsed Itrax results data
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param elementsonly if TRUE, only chemical elements are included. If FALSE, the data is passed unfiltered, otherwise a character vector of desired variable names can be supplied
#' @param zeros if "addone", adds one to all values. If "limit", replaces zero values with 0.001. Otherwise a function can be supplied to remove zero values.
#' @param transform binary operator that if TRUE will center-log-transform the data, if FALSE will leave the data untransformed. Otherwise, a function can be supplied to transform the data.
#' @param divisions the number of samples to slice into
#'
#' @importFrom tidyr drop_na
#' @importFrom stats prcomp hclust dist cutree
#' @importFrom compositions clr
#' @importFrom rlang .data
#'
#' @return either an output of \code{prcomp()}, or a list including the input data
#'
#' @examples
#' itrax_section(CD166_19_S1$xrf)
#'
#' @export
#'

itrax_section <- function(dataframe,
                              divisions = 30,
                             elementsonly = TRUE,
                             zeros = "addone",
                             transform = TRUE){

  # fudge to stop check notes
  . = NULL
  group = NULL
  ids = NULL

  # label with ids
  dataframe$ids <- 1:dim(dataframe)[1]
  input_dataframe <- dataframe

  # trim to only the elements
  if(is.logical(elementsonly) == TRUE && elementsonly==TRUE){
    dataframe <- dataframe %>%
      select(any_of(c(periodicTable$symb, "ids")))
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))
  } else if(is.logical(elementsonly) == TRUE && elementsonly==FALSE){
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))
  } else{
    dataframe <- dataframe %>%
      select(any_of(c(elementsonly, "ids")))
    dataframe <- dataframe %>%
      select(which(!colSums(dataframe, na.rm = TRUE) %in% 0))}

  # deal with the zeros
  if(zeros=="addone"){
    dataframe <- dataframe + 1
    dataframe$ids <- dataframe$ids-1
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
      mutate(across(any_of(periodicTable$symb), ~compositions::clr(.)))
      #compositions::clr()
  } else if(is.logical(transform) == TRUE && transform==FALSE){
    dataframe <- dataframe
  } else{
    dataframe <- transform(dataframe)
  }

  #glimpse(dataframe)

  # perform the first ordering
  firstorder <- as_tibble(dataframe) %>%
    mutate(group = dataframe %>%
             as.matrix() %>%
             dist() %>%
             hclust(method = "ward.D2") %>%
             cutree(k=divisions) %>%
             as.factor()) %>%
    select(`ids`, `group`)

  # perform second ordering
  # subset a second order group

  rep_samples <- lapply(unique(firstorder$group), function(x){x

  second_order_subset <- as_tibble(dataframe) %>%
    mutate(group = firstorder$group) %>%
    filter(group == x)

  # perform another ordering of them and subset
  second_order_subset <- second_order_subset %>%
     mutate(group = second_order_subset %>%
             select(-`group`, `ids`) %>%
             as.matrix() %>%
             dist() %>%
             hclust(method = "ward.D2") %>%
             .$order
            ) %>%
    filter(group == round(mean(`group`))) %>%
    pull(`ids`)
  })

  rep_samples <- rep_samples %>%
    unlist()

  #glimpse(rep_samples)

  rep_samples <- input_dataframe %>%
    filter(`ids` %in% rep_samples) %>%
    select(-`ids`)

  #glimpse(rep_samples)

  # sort the return
  return(list(groups = right_join(firstorder, input_dataframe, by = "ids") %>%
                select(-`ids`),
              samples = rep_samples))
}

# TODO make plotting function here
# myPCA %>%
#  broom::augment(CD166_19_S1$xrf %>% tidyr::drop_na())
