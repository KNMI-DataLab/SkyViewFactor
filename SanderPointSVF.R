library(shadow)
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

R<-100

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


test<-over(GMS_meta,r.grid)
I.point<-which(!is.na(test))

coords<-coordinates(GMS_meta[I.point,])

x_min<-coords[1]-R
x_max<-coords[1]+R
y_min<-coords[2]-R
y_max<-coords[2]+R

crop_extent<-extent(c(x_min,x_max,y_min,y_max))

extCropped<-crop(r,crop_extent)
point.svf<-svf(extCropped,maxDist=200)
#################################################################
########Cropping for a circle using raster and rgeos packages
#################################################################
#install rgeos for the gBuffer function

points <- as(GMS_meta[I.point,],"SpatialPoints")
pbuf <- gBuffer(points, widt=R)
buf <- mask(r, pbuf)
buffer <- trim(buf, pad=2)

plot(buf)
plot(buffer)
#################################################################
#################################################################
# sp_gms<-as(GMS_meta[I.point,],"SpatialPoints")

# extCropped<-crop(ext,r)


# SVF(location = sp_gms,
    # obstacles = extCropped, 
    # obstacles_height_field = "layer")
