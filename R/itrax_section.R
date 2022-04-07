#' Cluster analysis and statistical grouping of Itrax data
#'
#' Performs a cluster analysis and automatic statistical grouping of parsed Itrax results data to n groups.
#' Also provides information on the most "representative" (central) of each group. These can be used to develop a
#' sub-sampling regime for calibration using another method.
#'
#' @param dataframe pass the name of a dataframe parsed using \code{"itrax_import()"} or \code{"itrax_join()"} or \code{"itrax_reduce()"}.
#' @param elementsonly if TRUE, only chemical elements are included. If FALSE, the data is passed unfiltered, otherwise a character vector of desired variable names can be supplied.
#' @param zeros if "addone", adds one to all values. If "limit", replaces zero values with 0.001. Otherwise a function can be supplied to remove zero values.
#' @param transform binary operator that if TRUE will center-log-transform the data, if FALSE will leave the data untransformed. Otherwise, a function can be supplied to transform the data.
#' @param divisions the number of groups to slice into - also the number of representative samples returned.
#' @param plot set to true if a summary plot is required as a side-effect - the input dataset must have a depth or position variable - depth is used preferentially.
#'
#' @importFrom tidyr drop_na
#' @importFrom compositions clr
#' @importFrom rlang .data
#' @importFrom stats hclust cutree
#'
#' @return the input data with additional columns `group` and `calib_sample`, and possibly `uid` if not supplied.
#'
#' @examples
#' itrax_section(CD166_19_S1$xrf, plot = TRUE)
#' itrax_section(CD166_19_S1$xrf %>% itrax_reduce(by = 10), plot = TRUE)
#'
#' @export
#'

itrax_section <- function(dataframe,
                          divisions = 30,
                          elementsonly = TRUE,
                          zeros = "addone",
                          transform = TRUE,
                          plot = FALSE){

  # bind some variables to stop build notes
  #uid <- group <- value <- NULL
  calib_sample <- NULL
  . <- NULL
  uid <- NULL

  # copy the original data
  original_data <- dataframe
  n <- divisions

  # label it with ids
  # if there are already uids, and they are unique, it will just use them
  original_data <- uid_labeller(original_data)

  # make a transformed version of the data
  transformed_data <- original_data %>%
    multivariate_import(elementsonly = elementsonly,
                        zeros = zeros,
                        transform = transform) %>%
    compositions::clr()

  # perform first pass work
  firstgroups <- left_join(original_data,
                           tibble(uid = transformed_data %>%
                                    rownames(),
                                  group = dist(transformed_data) %>%
                                    hclust(method = "ward.D2") %>%
                                    cutree(k = n) %>%
                                    as.factor()
                           ),
                           by = "uid")

  # perform second pass work - start by splitting
  split_groups_full <- firstgroups %>%
    select(.data$uid, .data$group) %>%
    tidyr::drop_na() %>%
    group_by(.data$group) %>%
    group_split()

  # we need to remove any groups with only one observation in, becuase they are going to fail on hclust()
  # but first check this is the situation
  if( !length(split_groups_full[ which(lapply(split_groups_full, nrow) %>% unlist() == 1) ]) == 0){
    split_groups <- split_groups_full[ -which(lapply(split_groups_full, nrow) %>% unlist() == 1) ]
  } else{split_groups <- split_groups_full}

  # now do the second pass grouping
  samples <- lapply(seq(from = 1, to = length(split_groups)),
                    function(x){
                      transformed_data[split_groups[[x]] %>% pull(.data$uid),] %>%
                        dist() %>%
                        hclust(method = "ward.D2") %>%
                        `[[`("labels") %>%
                        `[[`(length(.)/2)
                    }) %>%
    unlist() %>%
    as_tibble() %>%
    rename(uid = .data$value) %>%
    mutate(calib_sample = TRUE)

  # deal with the special case of groups with only one observation
  # it identifies groups with only one observation
  # appends them to the regular list
  # then deletes the weird values generated previously from groups of only one observation
  # but first check if that even needs to be done

  if( !length(split_groups_full[ which(lapply(split_groups_full, nrow) %>% unlist() == 1) ]) == 0){

    samples <- suppressMessages(
      full_join(samples,
                tibble(uid = split_groups_full[ which(lapply(split_groups_full, nrow) %>% unlist() == 1) ] %>%
                         lapply(., function(x){select(x, "uid")}) %>%
                         unlist(),
                       calib_sample = TRUE)#,
                #by = c("uid", "calib_sample")
      )
    ) %>%
      filter() %>%
      mutate(uid = as.character(uid))
  }

  # create the output object
  df <- left_join(firstgroups, samples, by = "uid") %>%
    tidyr::replace_na(list(calib_sample = FALSE))

  # sideplot if required
  if(plot == TRUE && !"depth" %in% colnames(df)){
    print(
      df %>%
        ggplot(aes(x = .data$position, y = 1, fill = .data$group)) +
        geom_tile() +
        scale_x_reverse() +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") +
        geom_rug(sides = "b", data = filter(df, calib_sample == TRUE))
    )

  } else if(plot == TRUE && "depth" %in% colnames(df)){
    print(
      df %>%
        ggplot(aes(x = .data$depth, y = 1, fill = .data$group)) +
        geom_tile() +
        scale_x_reverse() +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") +
        geom_rug(sides = "b", data = filter(df, calib_sample == TRUE))
    )

  }

  # create the output
  return(df)
}
