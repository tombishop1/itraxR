#' Import an Itrax Scan Object
#'
#' Brings together all of the data in an Itrax Core Scan in a single object.
#'
#' @param image name of the image file
#' @param imagemetadata location of the metadata associated with the image scan
#' @param radiograph location of the raw radiograph file
#' @param radiographmetadata location of the metadata associated with the radiograph scan
#' @param xrf location of the xrf results file
#' @param xrfmetadata location of the metadata associated with the xrf scan
#' @param spectra location of the xrf spectra files
#' @param settings location of the settings file associated with the xrf scan
#'
#' @return a object of all the data, where for scan data the row names are positions on the scanner in mm
#'
#' @examples
#' itrax_corescan()
#'
#' @export
#'

# establish the function
itrax_corescan <- function(image="WV17-2-D1_rgb_16 bit.tif", imagemetadata="document.txt",
                           radiograph="radiograph.raw", radiographmetadata="document.txt",
                           xrf="result.txt", xrfmetadata="document.txt",
                           spectra="XRF data", settings="settings_240117.dfl"){

  #import the image
  if(!is.null(image)){
    image <- itrax_image(filename = image, metadata = imagemetadata)
  }

  # import the radiograph
  if(!is.null(radiograph)){
    radiograph <- itrax_radiograph(filename = radiograph, metadata = radiographmetadata)
  }

  # import the elemental data
  if(!is.null(xrf)){

    # create the import function
    itrax_xrf <- function(filename, metadata){
      # import the data
      df <- read.table(filename, skip=2, header=TRUE, sep = "\t")
      # create the position index
      row.names(df) <- as.numeric(df[ , "position..mm." ])
      # return the data
      return(df)
    }

    # use the import function
    xrf <- itrax_xrf(filename = xrf, metadata = xrfmetadata)
  }

  # import the spectral data
  if(!is.null(spectra)){
    spectra <- itrax_restspectra(foldername = spectra)
  }

  # import and create the metadata
  opticalmetadata <- itrax_meta()
  radiographmetadata <- itrax_meta()
  xrfmetadata <- itrax_meta()

  metadata <- list( "optical" = opticalmetadata,
                    "radiograph" = radiographmetadata,
                    "xrf" = xrfmetadata)

  # import the settings data
  if(!is.null(settings)){
    # create the function
    itrax_settings <- function(filename = "settings.dfl"){
      # read in the detector parameters
      detparams <-  read.delim(filename, skip = 1, nrows = 11, header = FALSE, sep = "=")
      # read in the tube parameters
      tubeparams <- read.delim(filename, skip = 13, nrows = 13, header = FALSE, sep = "=")
      # read in the fit cluster
      fitcluster <- read.delim(filename, skip = 27, nrows = 10, header = FALSE, sep = "=")
      # read in the rows 39 to 141 as the elements enabled
      elements <- read.delim(filename, skip = 38, nrows = 103, header = FALSE, sep = "=")
      # read in the rows from 143 onwards as extra lines
      lines <- read.delim(filename, skip = 142, header = FALSE, sep = "=")
      # make them into an object and return it
      return(list( "detector_parameters" = detparams,
                   "tube_parameters" = tubeparams,
                   "fit_parameters" = fitcluster,
                   "elements_enabled" = elements,
                   "extra_lines" = lines))
    }

    # use the function
    settings <- itrax_settings(settings)
  }

  # return the data in an object
  return(list( "image"=as.array(image),
               "radiograph"=as.matrix(radiograph),
               "xrf"=c(xrf),
               "spectra"=as.matrix(spectra),
               "metadata"=c(metadata),
               "settings"=c(settings) ) )
}
