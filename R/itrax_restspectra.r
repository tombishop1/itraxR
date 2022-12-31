#' Make a spectrograph from raw Itrax data spectra files
#'
#' Parses a folder full of raw spectra files from an Itrax core scanner and produces a spectral graph of all the data by position
#'
#' @param foldername defines the folder where the spectra \code{"*.spe"} files are located - or the path of the zipped folder where it is stored.
#' @param parameters optionally, defines the Q-Spec settings file from which to calculate the channel energies
#' @param datapos defines the row at which spectral data begins in the files
#' @param plot TRUE/FALSE, selects whether to create a plot as a side-effect
#' @param trans transformation applied in the plot - see `?ggplot2::scales_colour_gradient()` for options
#'
#' @return a dataframe of all the spectral data
#'
#' @examples
#' \dontrun{itrax_restspectra("~/itraxBook/CD166_19_(2020)/CD166_19_S1/CD166_19_S1/XRF data")}
#'
#' @import dplyr ggplot2 readr
#' @importFrom rlang .data
#' @importFrom stringr str_which
#' @importFrom utils unzip
#'
#'
#' @export

# function for integrating raw xrf spectra and visualising the same
itrax_restspectra <- function(foldername = "XRF data",
                              parameters = "settings.dfl",
                              datapos = 37,
                              plot = TRUE,
                              trans = "pseudo_log") {

  energy <- NULL
  name <- NULL
  position <- NULL
  value <- NULL
  filename <- NULL
  channel <- NULL

  # import the parameters
  if(file.exists(parameters) == TRUE){
    settings <- itrax_qspecsettings(parameters)
    channel_kev <- as.numeric(pull(settings[stringr::str_which(settings$key, pattern = "keV/channel"), "value"]))
    channel_offset <- as.numeric(pull(settings[stringr::str_which(settings$key, pattern = "energy offset"), "value"]))
    rm(settings)}

  if(dir.exists(foldername)){
    # read in a list of files
    filenames <- dir(foldername, pattern="*.spe")
    filenames <- paste(foldername, filenames, sep="/")
  } else if(file.exists(foldername)){
    filenames <- unzip(zipfile = foldername, list = TRUE)$Name
  } else{stop("The folder or *.zip file doesn't exist.")}

  # import spectral data
  if(dir.exists(foldername)){
    tables <- suppressWarnings(lapply(filenames,
                                      itrax_spectra,
                                      plot = FALSE)
    )
  } else if(file.exists(foldername)){
    tables <- lapply(filenames,
                     function(x){itrax_spectra(unz(description = foldername,
                                                   filename = x),
                                               plot = FALSE)
                     }
    )
  }

  # import all the scan positions
  if(dir.exists(foldername)){
    depths <- suppressWarnings(lapply(filenames,
                                      read.table,
                                      skip = 6,
                                      nrows = 1,
                                      header = FALSE)
    )
  } else if(file.exists(foldername)){
    depths <- lapply(filenames,
                     function(x){read.table(unz(description = foldername,
                                                filename = x),
                                            skip = 6,
                                            nrows = 1,
                                            header = FALSE)}
    )
  }

  # label all the dataframes in the list
  names(tables) <- unname(sapply(depths, `[[`, 2))

  # make an array where x=position, y=channel, and the value (color) = count
  df <- unname(sapply(tables, `[[`, "count"))

  # transpose
  foo <- t(df)

  # make tibble
  foo <- suppressMessages(as_tibble(foo, .name_repair = "universal"))

  # change the variables names
  if(exists("channel_offset") && exists("channel_kev")){
    colnames(foo) <- (1:dim(foo)[2] * channel_kev) + channel_offset
  } else{
    colnames(foo) <- 1:dim(foo)[2]
  }

  # create the variable for depth and filename
  foo <- foo %>%
    mutate(filename = filenames,
           position = unname(sapply(depths, `[[`, 2))) %>%
    select(filename, position, everything())

  # draw a plot as a side-effect if required
  if(plot == TRUE){
    p <- foo %>%
      tidyr::pivot_longer(cols = c(-filename, -position)) %>%
      mutate(name = as.numeric(name)) %>%
      rename(channel = name) %>%
      select(-filename) %>%

      ggplot(aes(x = channel, y = position, fill = value)) +
      geom_raster() +
      scale_fill_gradient(name = "value",
                          trans = trans,
                          low = "#132B43",
                          high = "#56B1F7",
                          labels = round) +
      scale_y_reverse() +
      ylab("position [mm]") +
      guides(fill = "none")

    if(exists("channel_offset") && exists("channel_kev")){
      p <- p + xlab("energy [k eV]") +
        scale_x_continuous(sec.axis = sec_axis(trans = ~ (. / channel_kev) - channel_offset,
                                               name = "channel [n]"))
    } else{
      p <- p + xlab("channel [n]")
    }

    print(p)

  }

  # returns
  return(foo)
}
