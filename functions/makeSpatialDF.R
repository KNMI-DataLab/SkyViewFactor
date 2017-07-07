#' Makes a spatial dataframe from a normal dataframe and a projection
#' 
#' \code{makeSpatialDF} makes a spatial dataframe from a normal dataframe and projection. It is assumed that the
#' coordinates are in the X and Y components. 
#' 
#' @seealso This function is called in \code{\link{SVF}}
#' @param df dataframe
#' @param projection projection CRS object
#' 
#' 
#' 
#' @export


makeSpatialDF <- function(df,projection){
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  df
}