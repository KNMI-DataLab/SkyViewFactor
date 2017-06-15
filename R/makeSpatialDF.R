#'Make a spatialpointsdataframe
#'@title Create a SpatialPointDataFrmae
#'@param df a dataframe with X and Y as coordinates
#'@param projection the projection to add to the dataframe
#'@export
makeSpatialDF <- function(df,projection){
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  df
}