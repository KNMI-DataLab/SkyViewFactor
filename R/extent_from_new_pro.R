#' Extent from new projection
#' @title New Extent
#' @param ext the extent in the current projection
#' @param pro.old the current projection
#' @param pro.new the new projection
#' @export

extent_from_new_pro<-function(ext,pro.old=WGS84,pro.new=pro){
  r<-raster(ext)
  crs(r)<-pro.old
  
  spdf<-as(r,"SpatialPointsDataFrame")
  spdf<-spTransform(spdf,pro.new)
  ext<-extent(spdf)
  return(ext)
}