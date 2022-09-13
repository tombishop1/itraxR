#' Convert an Itrax Image File into Munsell Colour
#'
#'  Reads a colour calibrated Itrax image file and processes it to estimate Munsell colour. 
#'
#' @param image defines the name of the image file imported using `itrax_image()`. It is essential that the image has been colour calibrated using a colour card or other method. 
#' @param proportion defines the width down the centre of the image to use for processing
#'
#' @return a table of values
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
#'             plot = FALSE) |> 
#'             itrax_munsell() |>
#'             sample(size = 10)
#'
#'
#' @importFrom munsellinterpol RGBtoMunsell
#' @importFrom munsellinterpol ColorBlockFromMunsell
#'
#' @export
#'

itrax_munsell <- function(image,
                          proportion = 0.1){
  apply(image[,round(dim(image)[2]/2-dim(image)[2]*proportion/2):round(dim(image)[2]/2+dim(image)[2]*proportion/2),], 
        c(1,3), 
        mean
  ) |> 
    munsellinterpol::RGBtoMunsell(maxSignal = 1) |> 
    munsellinterpol::ColorBlockFromMunsell()
}
