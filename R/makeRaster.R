makeRaster<-function(spatialDF, xres, yres, pro){
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(spatialDF)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(spatialDF,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid
  rm(dummyRaster)
  gc()
  return(r)
}