#' Read an Itrax Image File
#'
#'  Reads an Itrax image file and trims it according to the metadata provided.
#'
#' @param filename defines the name of the datafile to parse
#' @param metadata defines the relating metadata
#' @param plot     would you like to create a plot as a side-effect?
#'
#' @return a matrix of RGB values, and the relevant data from the metadata file relating to the image.
#' Also computes the aspect ratio of the image.
#'
#' @examples
#' itrax_image(file = "optical.tif", meta = "document.txt", plot = TRUE)
#'
#' @export
#'
itrax_image <- function(file = "optical.tif",
                        meta = "document.txt",
                        plot = FALSE){

  # import the image and metadata files
  require(tiff)
  image <- readTIFF(file)
  meta  <- itrax_meta(meta)

  # label the image depth
  image_depth <- rev(seq(from       = as.numeric(meta[ 9, 2]),
                         to         = as.numeric(meta[10, 2]),
                         length.out = dim(image)[2]))

  # trim the scan image
  image <- image[ , which(image_depth > as.numeric(meta[6, 2]) & image_depth < as.numeric(meta[7, 2])) , ]

  # rotate it
  image <- aperm(image, c(2, 1, 3))
  image <- image[c(dim(image)[1]: 1), , ]

  # return the data or process the image grob
  if(plot == TRUE){
    require(ggplot2)
    require(grid)
    print(ggplot() +
      ylim(rev(as.numeric(meta[6:7, 2]))) +
      scale_x_continuous(limits = c(0, (((as.numeric(meta[7, 2]) - as.numeric(meta[6, 2])) / dim(image)[1]) * dim(image)[2]))) +
      labs(y = "Position [mm]", x = "[mm]") +
      annotation_custom(rasterGrob(image,
                                   width  = unit(1, "npc"),
                                   height = unit(1, "npc")),
                        ymax = as.numeric(meta[7, 2])/-1,
                        ymin = as.numeric(meta[6, 2])/-1,
                        xmin = 0,
                        xmax = (((as.numeric(meta[7, 2]) - as.numeric(meta[6, 2])) / dim(image)[1]) * dim(image)[2])
      ))
 }

  return(list(image = image,
              meta  = meta[6:11, ],
              ratio = dim(image)[2]/dim(image)[1]
              )
         )
}
