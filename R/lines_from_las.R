#' Calculate pahts from a raster
#' 
#' \code{calculate_path} calculates the path for given distance, angles and coordinates from a raster file. A test file in
#' the folder test_data with test parameters is included. 
#' 
#' @seealso This function calls \code{\link{calculate_path}}
#' @param LAS location of the las file to read
#' @param pro projection, now only 28992 (RDcoordinates) is excepted 
#' @param xres x-resolution to grid the las points
#' @param yres y-resolution to grid the las points
#' @param field field to grid
#' @param point location for which the lines are calculated
#' @param R radius for which the line is calculated
#' @param Runcertainty can be used if the location coordinates are uncertain or if you want a surrounding area
#' @param theta angles for which the path is calculated
#' 
#' 
#' 
#' @export

lines_from_las<-function(LAS=testTile,pro=CRS("+init=epsg:28992"),
                         xres=5,yres=5,field="Z",
                         point=testPoint,R=200,Runcertainty=10,theta=c(5,10,30)){
  #1 Read the LAS
  message("reading LAS file")
  spPointTile<-readLAS(testTile)
  df<-data.frame(spPointTile)
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  
  #2 Make a raster from the LAS
  message("rasterizing Point data")
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(df)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(df,dummyRaster,field=field) #rasterizing the spatial points to a 1x1 grid
  #r.grid<-as(r,"SpatialGridDataFrame")
  
  #3 Cut the radius out of the raster
  message("Masking raster")
  cut.svf.buf<-gBuffer(point, width = Runcertainty)
  pbuf <- gBuffer(point, width = R+Runcertainty)
  
  buf <- mask(r, pbuf)
  #buffer <- raster::trim(buf)
  
  buffer.svf<-mask(buf,cut.svf.buf)
  buffer.svf<-raster::trim(buffer.svf)
  
  #4 Get the lines out the buffered grid
  message("Creating lines")
  xy<-data.frame(point)
  x<-as.numeric(xy[1])
  y<-as.numeric(xy[2])
  radius<-seq(from = 0, to = R+Runcertainty - 2 , by = 2)
  
  #5 Return vector with height 
  message("Extracting values")
  XY<-lapply(theta=theta,calculate_path,x=x,y=y,distance=radius,rbuffer=buffer.svf)
  return(XY)
}