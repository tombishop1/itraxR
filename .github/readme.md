# itraxR

Tools for Parsing and Analysing Itrax Core Scanner Data.

## Functionality

The Itrax Core Scanner (Cox Analytical Systems, Sweden) is a multi-sensor device for analysing geological and sediment cores. It produces high-resolution line-scan photography, line scan x-radiographs, and energy-dispersive x-ray florescence elemental composition analysis. The scanner is non-destructive, automated and capable of high resolution (~0.2 mm) down-core resolution. 

The datasets produced by the scanner can be large and complex. This software, written for the `R` environment, will assist users in parsing the data, including tasks like integrating multiple, possibly overlapping core sections, repeat scans of core sections and assigning depth information to the scan data.

It has functions to assist with plotting image and compositional data, as well as visualising raw scan data. It also has functions for performing various common multivariate analyses on the compositional data, like PCA and clustering. 

## Extended Manual

There is an extended manual available at https://tombishop1.github.io/itraxBook/ and a complete example dataset is available from https://github.com/tombishop1/itraxBook/. 

## Issues

This package is in development and all pull requests, issues, corrections or feature requests are gratefully received. 

## Citation

If you have used this package as part of your publication, remember to cite the package; you can use `citation("itraxR")` from your `R` terminal for the correct citation. 

## Contact

This package is authored by Tom Bishop --- you can get in touch via my website, https://thomasbishop.uk/ or use the Issues page if appropriate. 
