## code to prepare `DATASET` dataset goes here

CD166_19_S1 <- list(xrf = itrax_import(filename = system.file("extdata", "CD166_19_S1_Results.txt", package = "itraxR"),
                                       depth_top = 0),
                    rgb = itrax_image(file = system.file("extdata", "CD166_19_S1_optical_lowres.tif", package = "itraxR"),
                                      meta = system.file("extdata", "CD166_19_S1_xrf_document.txt", package = "itraxR")),
                    rad = itrax_radiograph(file = system.file("extdata", "CD166_19_S1_radiograph_adj.tif", package = "itraxR"),
                                           meta = system.file("extdata", "CD166_19_S1_rad_document.txt", package = "itraxR"))#,
                    #meta = itrax_meta(datafile = system.file("extdata", "CD166_19_S1_xrf_document.txt", package = "itraxR"))
                    )

usethis::use_data(CD166_19_S1, overwrite = TRUE)
