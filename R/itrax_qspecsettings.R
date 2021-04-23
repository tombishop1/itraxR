#' Read a Q-Spec settings file and parse the key-value pairs
#'
#' This is used to retrieve settings important elsewhere, for example the mca bin width and offset
#'
#' @param filename the *.dfl settings file that relates to the rest of the data
#'
#' @return a tibble of the parsed data
#'
#' @examples
#' itrax_qspecsettings(filename = system.file("extdata",
#'                                 "Results_settings.dfl",
#'                                 package = "itraxR",
#'                                 mustWork = TRUE)
#'                                 )
#'
#' @importFrom rlang .data
#' @importFrom stringr str_subset str_split str_trim
#'
#' @export

itrax_qspecsettings <- function(filename = "Results_settings.dfl"){
  tibble(key = scan(file = filename,
                    what = "",
                    sep = "\n",
                    quiet = TRUE) %>%
           stringr::str_subset("\\[", negate = TRUE) %>%
           stringr::str_split(pattern = "=") %>%
           sapply(`[`, 1) %>%
           stringr::str_trim(side = "both"),
         value =  scan(file = filename,
                       what = "",
                       sep = "\n",
                       quiet = TRUE) %>%
           stringr::str_subset("\\[", negate = TRUE) %>%
           stringr::str_split(pattern = "=") %>%
           sapply(`[`, 2) %>%
           stringr::str_trim(side = "both")
  )
}
