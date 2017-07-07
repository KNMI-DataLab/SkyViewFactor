#' Makes a raster version of a spatial dataframe
#' 
#' \code{makeRaster} makes a raster representation of a spatial dataframe given the resolution and projection.
#' 
#' @seealso This function is called in \code{\link{SVF}}
#' @param spatialDf spatial dataframe
#' @param xres x-axis resolution
#' @param yres y-axis resolution
#' @param pro projection CRS object
#' 
#' 
#' 
#' @export

makeRaster<-function(spatialDF, xres, yres, pro){
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(spatialDF)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(spatialDF,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid
  rm(dummyRaster)
  gc()
  return(r)
}