library(raster)

r<-raster("~/SVFtoExamine/layer_tile10_kdcVers.nc", varname = "svf")
proj4string(r)=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs")
#projection(r)<-CRS("+init=epsg:28992")



library(leaflet)
library(sp)
e <- as(raster::extent(173000, 174000, 443000, 444000), "SpatialPolygons")
#proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#plot(e)

#5.640106,51.957173,5.694008,51.984669


test<-crop(r,e)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(test),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(test, colors = pal, opacity = 0.5)




####DEM from point cloud rasterized
lazFolder <- c("/data1", "/data2", "/data3")


files<-list.files(path = lazFolder, pattern = paste0("ahn_173000_443000.laz"), full.names = T, recursive = T)

library(rLiDAR)

lasData<-readLAS("~/testForAlign/ahn_173000_443000.las")

DF<-data.frame(lasData)
pro<-CRS("+init=epsg:28992")
DFSpatial<-makeSpatialDF(DF,projection = pro)
Xres<-1
Yres<-1
DFraster<-makeRaster(DFSpatial,Xres,Yres,pro)

pal2 <- colorNumeric("Blues", range(-10,25),na.color = "transparent")

leaflet() %>% addTiles() %>% addRasterImage(DFraster, colors = pal2, opacity = 0.7)



###SVF tile only AWS
AWSraster<-stack("~/testForAlign/fromAWS/173000_443000.grd")

pal3 <- colorNumeric("Blues", range(0,1),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(AWSraster[[2]], colors = pal3, opacity = 0.5)



###SVF grouped tile AWS
AWSrasterFull<-raster("~/testForAlign/fromAWS/layer_tile10.grd")

test2<-crop(AWSrasterFull,e)


pal4 <- colorNumeric("Blues", range(0,1),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(test2, colors = pal4, opacity = 0.5)


#test of re-created NETCDF
netcdfReComp<-raster("/home/pagani/testForAlign/fromAWS/layer_tile10.nc")
#proj4string(netcdfReComp)=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs")
proj4string(netcdfReComp)=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
#projection(netcdfReComp)<-CRS("+init=epsg:28992")
crs(netcdfReComp)<-crs(AWSrasterFull)

test3<-crop(netcdfReComp,e)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(test3),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(test3, colors = pal, opacity = 0.5)


#test of re-created NETCDF2
library(ncdf4)
netcdfReComp2<-raster("/home/pagani/testForAlign/fromAWS/secondTEST.nc")
#CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs")
#proj4string(netcdfReComp2)=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
#projection(netcdfReComp2)<-CRS("+init=epsg:28992")
#crs(netcdfReComp)<-crs(AWSrasterFull)
crs(netcdfReComp2)<-CRS("+init=epsg:28992")


test3<-crop(netcdfReComp2,e)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(test3),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(test3, colors = pal, opacity = 0.5)

#TIFF
tiff<-raster("/home/pagani/testForAlign/fromAWS/thirdTEST.tif")
crs(tiff)<-CRS("+init=epsg:28992")

test4<-crop(tiff,e)

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(test4),na.color = "transparent")
leaflet() %>% addTiles() %>% addRasterImage(test4, colors = pal, opacity = 0.5)
