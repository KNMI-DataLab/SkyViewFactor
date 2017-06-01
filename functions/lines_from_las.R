lines_from_las<-function(LAS=testTile,pro=CRS("+init=epsg:28992"),
                         xres=5,yres=5,field="Z",
                         point=testPoint,R=200,Runcertainty=10,theta=c(5,10,30)){
  
  if(inherits(point,'sp')){
    warning('No spatial point input, returning NULL')
    return(NULL)
  }
  
  #1 Read the LAS
  message("reading LAS file")
  spPointTile<-readLAS(LAS)
  df<-data.frame(spPointTile)
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  
  if (is.null(df)){
    warning(sprintf('Nothing in ?LAS  file %s \n',LAS))
    return(NULL)
  }
  
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
  buffer <- raster::trim(buf)
  
  buffer.svf<-mask(buf,cut.svf.buf)
  buffer.svf<-raster::trim(buffer.svf)
  
  #4 Get the lines out the buffered grid
  message("Creating lines")
  xy<-data.frame(point)
  x<-as.numeric(xy[1])
  y<-as.numeric(xy[2])
  distance<-seq(from = 0, to = R, by = 2)
  # radius<-seq(from = 0, to = R, by = 2)
  #5 Return vector with height 
  message("Extracting values for the angles %s \n",theta)
  XY<-lapply(theta,calculate_path,x=x,y=y,distance=distance)
  return(XY)
}