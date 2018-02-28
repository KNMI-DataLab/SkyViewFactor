library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(uuid)
library(data.table)
library(stringr)
library(spatial.tools)


raster_dir2<-"/data1/lidarTilesGTiff_2m/"
raster_dir3<-"/data1/lidarTilesGTiff_3m/"
raster_dir4<-"/data1/lidarTilesGTiff_4m/"
raster_dir10<-"/data1/lidarTilesGTiff_10m/"
raster_dir20<-"/data1/lidarTilesGTiff_20m/"
raster_dir50<-"/data1/lidarTilesGTiff_50m/"



#raster_dir_bird<-"/data1/lidarTilesDeBilt1m/"





listGRDTiles2 <- list.files(path = raster_dir2, pattern = ".tif", full.names = T, recursive = T)
listGRDTiles3 <- list.files(path = raster_dir3, pattern = ".tif", full.names = T, recursive = T)
listGRDTiles4 <- list.files(path = raster_dir4, pattern = ".tif", full.names = T, recursive = T)
listGRDTiles10 <- list.files(path = raster_dir10, pattern = ".tif", full.names = T, recursive = T)
listGRDTiles20 <- list.files(path = raster_dir20, pattern = ".tif", full.names = T, recursive = T)
listGRDTiles50 <- list.files(path = raster_dir50, pattern = ".tif", full.names = T, recursive = T)



#listGRDTiles<-list(listGRDTiles)
#commented out for the single test of the timing
test2<-lapply(listGRDTiles2,raster)
merged2<- do.call(merge, test2)

rasterOptions(tolerance = 1)


test3<-lapply(listGRDTiles3,raster)
merged3<- do.call(merge, c(test3, tolerance=1))

test4<-lapply(listGRDTiles4,raster)
merged4<- do.call(merge, test4)

test10<-lapply(listGRDTiles10,raster)
merged10<- do.call(merge, test10)

test20<-lapply(listGRDTiles20,raster)
merged20<- do.call(merge, test20)

test50<-lapply(listGRDTiles50,raster)
merged50<- do.call(merge, test50)


raster2m<-merged2
raster3m<-merged3
raster4m<-merged4
raster10m<-merged10
raster20m<-merged20
raster50m<-merged50




tasks<-list(
  svf100_2m = function() svf(raster2m, nAngles = 16, maxDist = 100, ll=F),
  svf100_3m = function() svf(raster3m, nAngles = 16, maxDist = 100, ll=F),
  svf100_4m = function() svf(raster4m, nAngles = 16, maxDist = 100, ll=F),
  svf100_10m = function() svf(raster10m, nAngles = 16, maxDist = 100, ll=F),
  svf100_20m = function() svf(raster20m, nAngles = 16, maxDist = 100, ll=F),
  svf100_50m = function() svf(raster50m, nAngles = 16, maxDist = 100, ll=F)
)



# Using fork()
out <- mclapply( 
  tasks, 
  function(f) f(), 
  mc.cores = 8 
)

for(i in 1:length(tasks)){
  writeRaster(out[[i]],paste0("/home/pagani/development/outputTilesTest/100mRsensResNew/",names(tasks[i])))
}






