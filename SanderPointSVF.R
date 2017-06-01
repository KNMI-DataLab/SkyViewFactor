library(raster)
library(rgdal)
library(rLiDAR)
library(horizon)
library(rgeos)
library(data.table)
pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

xres<-5 # x-resolution in meters
yres<-5 # y-resolution in meters

R<-200
Runcertainty<-10

GMS_meta<-fread("/home/pagani/development/SkyViewFactor/GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)

testTile<-"/ssd1/GMSsvf/ahn_261000_468000.las"
spPointTile<-readLAS(testTile)
df<-data.frame(spPointTile)

coordinates(df)<-~X+Y
proj4string(df)<-pro

dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
extent(dummyRaster)<-extent(df)
res(dummyRaster)<-c(xres,yres) # set the resolution
r<-rasterize(df,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid


# ext <-as(r, "SpatialPolygonsDataFrame")

r.grid<-as(r,"SpatialGridDataFrame")
# 
# 
test<-over(GMS_meta,r.grid)
I.point<-which(!is.na(test))
# 
# coords<-coordinates(GMS_meta[I.point,])
# 
# x_min<-coords[1]-R
# x_max<-coords[1]+R
# y_min<-coords[2]-R
# y_max<-coords[2]+R
# 
# crop_extent<-extent(c(x_min,x_max,y_min,y_max))
# 
# extCropped<-crop(r,crop_extent)
# point.svf<-svf(extCropped,maxDist=200)
#################################################################
########Cropping for a circle using raster and rgeos packages
#################################################################
#install rgeos for the gBuffer function

points <- as(GMS_meta[I.point,],"SpatialPoints")

cut.svf.buf<-gBuffer(points, width = Runcertainty)
pbuf <- gBuffer(points, width = R+Runcertainty)


buf <- mask(r, pbuf)
buffer <- raster::trim(buf)

plot(buf)
plot(buffer)

buffer.svf<-mask(buf,cut.svf.buf)
buffer.svf<-raster::trim(buffer.svf)

plot(buffer.svf)
#################################################################
#################################################################
#Extract lines
# xy<-data.frame(points)
# 
# theta<-20
# x<-as.numeric(xy[1])
# y<-as.numeric(xy[2])
# radius<-100
# 
# dx<-cos(theta)*radius
# dy<-sin(theta)*radius
# 
# x1<-x+dx
# y1<-y+dy
# 
# xy1<-data.frame(x1,y1)
# 
# X<-rbind(x,x1)
# Y<-rbind(y,y1)
# XY<-data.frame(X,Y)
# coordinates(XY)<-~X+Y
# # coordinates(xy1)<-~x1+y1
# # 
# # points1<-SpatialPoints(xy1)
# 
# 
# line<-spLines(XY)
# crs(line)<-crs(points)
# 
# plot(buffer)
# lines(line)
# 
# values<-raster::extract(buffer,line)


##################### INCREMENTAL RADIUS######################################
#Extract lines
xy<-data.frame(points)

theta<-30
x<-as.numeric(xy[1])
y<-as.numeric(xy[2])
radius<-seq(from = 1, to =200,by = 2)

dx<-cos(theta)*radius
dy<-sin(theta)*radius

x1<-x+dx
y1<-y+dy

xy1<-data.frame(x1,y1)

zero<-0

distance<-c(zero,radius)

XY<-rbind(c(x,y),xy1)
XY<-cbind(XY,distance)
XY<-data.frame(XY)
#coordinates(XY)<-~x1+y1
# coordinates(xy1)<-~x1+y1
# 
# points1<-SpatialPoints(xy1)


#line<-spLines(XY)
#crs(line)<-crs(points)

#plot(buffer)
#lines(line)

values<-raster::extract(buffer,XY[1:2])
XY<-cbind(XY,values)
plot(XY$distance,XY$values)
lines(XY$distance,XY$values)
#######################################################

testTile<-"/ssd1/GMSsvf/ahn_261000_468000.las"
testPoint<-readRDS("test_data/point.rds")
#########################################################
#TO DO
#1 for a single point, determine the tile/tiles
#########################################################
lines_from_las<-function(LAS=testTile,pro=CRS("+init=epsg:28992"),
                         xres=5,yres=5,field="Z",
                         point=testPoint,R=200,Runcertainty=10,angle=30){
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
  cut.svf.buf<-gBuffer(points, width = Runcertainty)
  pbuf <- gBuffer(points, width = R+Runcertainty)
  
  buf <- mask(r, pbuf)
  buffer <- raster::trim(buf)

  buffer.svf<-mask(buf,cut.svf.buf)
  buffer.svf<-raster::trim(buffer.svf)
  
  #4 Get the lines out the buffered grid
  message("Creating lines")
  xy<-data.frame(points)
  x<-as.numeric(xy[1])
  y<-as.numeric(xy[2])
  distance<-seq(from = 0, to = R , by = 2)
  
  dx<-cos(theta)*radius
  dy<-sin(theta)*radius
  
  x1<-x+dx
  y1<-y+dy
  
  xy1<-data.frame(x1,y1)
  
  XY<-rbind(c(x,y),xy1)
  XY<-cbind(XY,distance)
  XY<-data.frame(XY)
  #5 Return vector with height 
  message("Extracting values")
  values<-raster::extract(buffer,XY[1:2])
  XY<-cbind(XY,values)
  names(XY)<-c("x","y","distance","height")
  return(XY)
}