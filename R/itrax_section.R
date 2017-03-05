#' Cluster analysis and statistical grouping of Itrax data
#'
#' Performs a cluster analysis and automatic statistical grouping of parsed Itrax results data
#'
#' @param dataframe defines Itrax results data parsed using \code{"itrax_import()"} or \code{"itrax_join()"}
#' @param divisions defines the number of sub-groups to divide the data into. Use this to define the number of samples for calibration.
#' @param zeros can be either "addone" or "limit" --- this defines what to do with zero values when normalisating. Limit uses 0.001 as the zero value, add one adds one to all data
#' @param elements an optional list of elements to include, otheriwse, all elements present are used
#' @param graph a binary operator that if TRUE will produce a graphical representation of the results
#'
#' @return an object that includes the depth (or position) and the respective group, alongside a list of group center samples for choosing calibration samples
#'
#' @examples
#' itrax_section( df )
#'
#' @export

itrax_section=function(dataframe, divisions=30, zeros="addone", elements=c(NULL), graph=TRUE){

  # this function returns the following:
  # $samples - a list of suggested samples for analysis
  # $depth OR $position depending on what's present
  # $groups - the groups to go with $depth

  # import the data
  # assert the dataframe exists and import it
  if(is.data.frame(dataframe)){
    df <- dataframe
  } else{
    stop('Dataframe does not exist or object is not a dataframe.')
  }

  # rename the rows by depth
  if("depth" %in% colnames(df)) {
    rownames(df) <- round(df$depth)
    z_is <- "depth"
    # or position
  } else if("position" %in% colnames(df)) {
    rownames(df) <- round(df$position)
    z_is <- "position"
  } else{
    stop('Neither depth nor position is present.')
  }

  # get rid of anything that isn't an element
  element = c("H", "He",
              "Li", "Be", "B", "C", "N", "O", "F", "Ne",
              "Na", "Mg", "Al", "Si", "P", "S", "Cl",
              # "Ar",
              "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
              "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe",
              "Cs", "Ba",
              "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
              "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn",
              "Fr", "Ra",
              "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No",
              "Lr", "Unq", "Unp", "Unh", "Uns", "Uno", "Une", "Unn")
  # get rid of anything not in the list of elements above
  df <- df[ , which(names(df) %in% element)]

  # get rid of anything not in the specified list of elements
  if(!is.null(elements)){
    df <- df[ , which(names(df) %in% elements)]
  }else{}

  # deal with zero values
  if(zeros=="addone") {
    df <- df + 1
  } else if(zeros=="limit") {
    df[df == 0] <- 0.001
  } else{stop('zeros must be addone or limit')}

  # deal with NA values
  df <- na.omit(df)

  # centered log ratio transform the data
  require(chemometrics) # could also use "compositions" package
  df <- clr(df)

  # perform a cluster analysis using Euclidian distances
  d <- dist(as.matrix(df))
  hc <- hclust(d, method = "ward.D2")

  # draw a dendrogram
  if(graph==TRUE){
    #plot(hc)
    #dev.new()
  } else if(graph==FALSE){
  } else{stop('graph must be true or false')}

  # divide into groups
  groups <- cutree(hc, k=divisions)
  df$group <- groups

  # do a cluster analysis of each group individually
  sample_list <- sample_list <- c(1:as.numeric(divisions))
  # sample_list <- NA
  loop_group <- 1

  while( loop_group < divisions ){

    loop_d <- dist( as.matrix(df[ df$group == loop_group , ]) )
    loop_hc <- hclust( loop_d, method="ward.D2" )

    # convert the index number to a depth label
    loop_reorder <- data.frame(loop_hc$labels, loop_hc$order)
    loop_reorder <- loop_reorder[order(loop_reorder$loop_hc.order),]

    # pick the MIDDLE in each group
    loop_centerpoint <- round(length(loop_reorder$loop_hc.labels) / 2)

    loop_center <- as.character(loop_reorder[loop_centerpoint,1])


    # append the depth or position of each of those samples to a list
    sample_list[loop_group] <- loop_center
    #sample_list <- c(sample_list, loop_center)

    loop_group <- loop_group + 1
  }

  # sort the list of samples numerically for the sake of neatness
  sample_list <- sort(as.numeric(sample_list))

  # print the list for diagnostics
  print(sample_list)

  # draw a picture
  if(graph==TRUE){
    require(ggplot2)
    p <- ggplot(df, aes(row.names(df), fill=as.factor(groups))) + geom_bar()
    print(p)
  } else if(graph==FALSE){
  } else{stop('graph must be true or false')}

  # make a small table with the position OR depth, and put the groups there
  group_table <- as.data.frame(df$group)
  row.names(group_table) <- row.names(df)
  colnames(group_table) <- "group"

  if(z_is=="depth"){
    return(list( "groups"=c(group_table$group), "depth"=c(row.names(df)), "samples"=c(sample_list) ) )
  } else if(z_is=="position"){
    return(list( "groups"=c(group_table$group), "position"=c(row.names(df)), "samples"=c(sample_list) ) )
  }

}
