#' Import an individual spectra file
#'
#' Sometimes it is helpful to read an individual spectral file for diagnostics
#'
#' @param filename defines the name of the *.spe datafile from the core scanner to parse
#' @param parameters optionally defines a relevant Q-Spec settings file in order to compute the energy scale, otherwise channel numbers are reported
#' @param plot logical, if TRUE a side-plot is created
#'
#' @return a tibble of the parsed data
#'
#' @examples
#'  itrax_spectra(filename   = system.file("extdata",
#'                                       "L000676.spe",
#'                                       package = "itraxR",
#'                                       mustWork = TRUE),
#'                parameters = system.file("extdata",
#'                                         "Results_settings.dfl",
#'                                         package = "itraxR",
#'                                         mustWork = TRUE)
#'                )
#'
#' @import dplyr ggplot2
#' @importFrom stringr str_which
#' @importFrom readr read_delim
#' @importFrom rlang .data
#'
#' @export

itrax_spectra <- function(filename,
                          parameters = "settings.dfl",
                          plot = TRUE){

  # if the parameters file exists, use it
  if(file.exists(parameters) == TRUE){
    settings <- itrax_qspecsettings(parameters)
    channel_kev <- as.numeric(pull(settings[stringr::str_which(settings$key, pattern = "keV/channel"), "value"]))
    channel_offset <- as.numeric(pull(settings[stringr::str_which(settings$key, pattern = "energy offset"), "value"]))
    rm(settings)
  }

  # read the spectra file
  spectra <- suppressWarnings(readr::read_delim(file = filename,
                              delim = "\t",
                              skip = 37,
                              col_names = TRUE,
                              )) %>%
    select(channel, content) %>%
    rename(count = .data$content) %>%
    mutate(count = as.integer(count))

  # if the parameters file exists, report the energies
  if(file.exists(parameters) == TRUE){
    spectra <- spectra %>%
      mutate(energy = (.data$channel * channel_kev) + channel_offset)
  }

  # if requested, make a side plot
  if(plot == TRUE){
    if("energy" %in% names(spectra)){
      p <- ggplot(data = spectra, aes(x = .data$energy, y = .data$count)) +
        xlab("energy [k eV]")
    } else{
      p <- ggplot(data = spectra, aes(x = .data$channel, y = .data$count)) +
        xlab("channel [n]")
    }

    p <- p +
      geom_line() +
      scale_y_continuous(trans = "pseudo_log") +
      ylab("")

    print(p)
    }

  # return
  return(spectra)
  }
