#' Read an Itrax Image File
#'
#'  Reads an Itrax image file and trims it according to the metadata provided.
#'
#' @param file defines the name of the datafile to parse
#' @param meta defines the relating metadata
#' @param trim defines custom trim parameters. The default behaviour uses the limits from the metadata file. Set the false for no trimming, or set the position limits by passing a two element vector.
#' @param plot would you like to create a plot as a side-effect?
#'
#' @return a matrix of RGB values, and the relevant data from the metadata file relating to the image.
#'
#' @examples
#' itrax_image(file = system.file("extdata",
#'                          "CD166_19_S1_optical_lowres.tif",
#'                          package = "itraxR",
#'                          mustWork = TRUE),
#'             meta = system.file("extdata",
#'                          "CD166_19_S1_xrf_document.txt",
#'                          package = "itraxR",
#'                          mustWork = TRUE),
#'             plot = TRUE)
#'
#' @import ggplot2
#'
#' @importFrom tiff readTIFF
#' @importFrom grid rasterGrob
#'
#' @export
#'
itrax_image <- function(file = "optical.tif",
                        meta = "document.txt",
                        plot = FALSE,
                        trim = TRUE){

  # import the image and metadata files
  image <- tiff::readTIFF(file)
  meta  <- itrax_meta(meta)

  # rotate it
  image <- aperm(image, c(2, 1, 3))
  image <- image[c(dim(image)[1]: 1), , ]

  # label with positions in mm
  row.names(image) <- seq(from       = as.numeric(meta[ 9, 2]),
                          to         = as.numeric(meta[10, 2]),
                          length.out = dim(image)[1])
  colnames(image) <-  seq(from       = 0,
                         by          = (as.numeric(meta[10, 2]) - as.numeric(meta[9, 2])) / dim(image)[1],
                         length.out  = dim(image)[2])

  # trim the scan image
  # check length
  if(length(trim) == 2){
    image <- image[ which(as.numeric(rownames(image)) >= trim[1] & as.numeric(rownames(image)) <= trim[2]) , , ]
  }
  # check true
  else if(trim == TRUE){
    image <- image[ which(as.numeric(rownames(image)) >= as.numeric(meta[6, 2]) & as.numeric(rownames(image)) <= as.numeric(meta[7, 2])) , , ]
  }
  # check false
  else if(trim == FALSE){
  }
  # return error
  else{stop("If you define trim parameters, pass a two element numeric vector of the start and stop positions.")}

  # return the data or process the image grob
  if(plot == TRUE){
    print(ggplot() +
            scale_x_continuous(limits = range(as.numeric(colnames(image))),
                               breaks = range(as.numeric(colnames(image))),
                               labels = round(range(as.numeric(colnames(image))), 1)) +
            scale_y_continuous(limits = rev(range(as.numeric(rownames(image))))) +
            labs(y = "Position [mm]", x = "[mm]") +
            coord_fixed(ratio = 1) +
            annotation_custom(rasterGrob(image,
                                         width  = unit(1, "npc"),
                                         height = unit(1, "npc")),
                              ymax = max(as.numeric(rownames(image))),
                              ymin = min(as.numeric(rownames(image))),
                              xmin = min(as.numeric(colnames(image))),
                              xmax = max(as.numeric(colnames(image))))
    )
  }

  return(list(image = image, meta  = meta[6:11, ]))
}
