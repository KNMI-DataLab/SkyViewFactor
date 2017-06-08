extent_from_new_pro<-function(ext,pro.old=WGS84,pro.new=pro){
  r<-raster(ext)
  crs(r)<-pro.old
  
  spdf<-as(r,"SpatialPointsDataFrame")
  spdf<-spTransform(spdf,pro.new)
  ext<-extent(spdf)
  return(ext)
}