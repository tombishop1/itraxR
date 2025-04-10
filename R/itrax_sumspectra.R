utils::globalVariables(c(":=", "content", "channel"))

#' Create arbitrary sumspectra files
#'
#' Parses data from a zipped collection of *.spe files to generate arbitrary sumspectra files from a defined input list
#'
#' @param sumspectra_file defines the name of the `sumspectra.spe` file to copy the header from
#' @param input_zip_folder defines the zip file to search in for the chosen `*.spe` files
#' @param input_files a character vector of filenames in the format `Lnnnnnn.spe` to use in the function
#' @param output_file defines the filename of the new sumspectra file
#' @param method one of `mean` (useful for generating pseudo `*.spe` files for calibration) or `sum` (a more exact recreation of the `sumspectra.spe` format)
#'
#' @return a tab-delimited file in a format compatible with a `sumspectra.spe` file
#'
#' @examples
#' \dontrun{itrax_sumspectra(sumspectra = "CD166_19_S1/CD166_19_S1/sumspectra.spe",
#'                           input_zip_folder = "CD166_19_S1/CD166_19_S1/XRF Data.zip",
#'                           input_files = CD166_19_xrf %>%
#'                             select(filename, depth) %>%
#'                             filter(depth >= 100 & depth <= 105) %>%
#'                             mutate(filename = str_split_i(filename, 
#'                               pattern = "XRF data\\\\", 
#'                               i = 2)
#'                               ) %>% 
#'                               pull(filename),
#'                             output_file = "sumspectra_100_105.spe",
#'                             method = mean)}
#'
#' @import dplyr

#' @importFrom rlang .data
#' @importFrom stringr str_split_i
#' @importFrom readr write_file 
#' @importFrom readr read_delim
#' @importFrom plyr join_all
#' @importFrom tidyselect starts_with
#'
#' @export

itrax_sumspectra <- function(sumspectra_file = "CD166_19_S1/CD166_19_S1/sumspectra.spe",
                             input_zip_folder = "CD166_19_S1/CD166_19_S1/XRF Data.zip",
                             input_files = c("L000001.spe", "L000002.spe"),
                             output_file = "mySumspectra.spe",
                             method = mean
                             ){
  stringr::str_split_i(string = read_file(sumspectra_file),
              pattern = "\rchannel",
              1) %>%
    write_file(output_file)
  
  write_file(file = output_file,
             append = TRUE,
             "\rchannel\tcontent\t\t\r")
  
  lapply(input_files, 
         function(x){
           read_delim(unz(description = input_zip_folder,
                          filename = paste0("XRF data/", x)),
                      skip = 37,
                      col_types = "cn__",
           ) %>%
             rename(!!x := content)
         }
  ) %>%
    
    plyr::join_all(.data, by = "channel") %>% 
    
    rowwise() %>%
    mutate(count = round(method(c_across(ends_with("spe"))))) %>%
    
    select(channel, count) %>%
    
    write_delim(output_file, 
                delim = "\t",
                append = TRUE,
                col_names = FALSE
    )
  
}
