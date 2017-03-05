#' Reduce size of Itrax result data
#'
#' Resamples Itrax results files into smaller averaged ranges
#'
#' @param dataframe defines the name of the dataframe to reduce --- this should have been imported using \code{"itrax_import()"}
#' @param interval  defines the intervals at which to resample, in mm.
#'
#' @return a dataframe of the averaged Itrax data
#'
#' @examples
#' itrax_averaging(df, interval = 10)
#'
#' @export

itrax_averaging=function(dataframe, interval) {

  # dataframe     pass the name of a dataframe parsed using itrax_import
  # interval      pass the size of the interval in mm

  # assert the dataframe exists and import it
  if(is.data.frame(dataframe)){
    df <- dataframe
  } else{
    stop('Dataframe does not exist or object is not a dataframe.')
  }

  # check if depths are present and subsistute position if not
  if("depth" %in% colnames(df)) {
    position_subs <- FALSE
  } else if("position" %in% colnames(df)){
    df$depth <- df$position
    position_subs <- TRUE
  } else{
    stop('Neither depth nor columns are present. Did you parse you data in itrax_import?')
  }

  # work out the distance as row on row increment represents
  scaninterval <- df[2, "depth"] - df[1, "depth"]
  scaninterval <- round(scaninterval, digits=2)

  # change ai to represent the equivalent number of rows
  # round it so it is a whole number
  interval <- interval / scaninterval
  interval <- round(interval)

  # perform the binning operation
  df_avg          <- aggregate(df,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),mean)[-1]
  df_avg$depthmin <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),min)$x
  df_avg$depthmax <- aggregate(df$depth,list(rep(1:(length(df$depth)%/%interval+1),each=interval,len=length(df$depth))),max)$x

  # rename the columns if they need it
  if(position_subs==TRUE){
    df_avg$position <- df_avg$depth
    df_avg$positionmin <- df_avg$depthmin
    df_avg$positionmax <- df_avg$depthmax
    df_avg$depth <- NULL
    df_avg$depthmin <- NULL
    df_avg$depthmax <- NULL
  }else if(position_subs==FALSE){
  }

  return(df_avg)
}
