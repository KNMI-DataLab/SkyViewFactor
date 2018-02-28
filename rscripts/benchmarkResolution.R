library(microbenchmark)
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


dataDir<-"/home/ubuntu/efs/tilesBenchmarkCompTime/toShipAWS_SVF_benchmark"

raster_dir2<-paste0(dataDir,"/lidarTilesGTiff_2m/")
raster_dir3<-paste0(dataDir,"/lidarTilesGTiff_3m/")
raster_dir4<-paste0(dataDir,"/lidarTilesGTiff_4m/")
raster_dir10<-paste0(dataDir,"/lidarTilesGTiff_10m/")
raster_dir20<-paste0(dataDir,"/lidarTilesGTiff_20m/")
raster_dir50<-paste0(dataDir,"/lidarTilesGTiff_50m/")
raster_dir1<-paste0(dataDir,"/lidarTilesGTiff_1m/")
raster_dir5<-paste0(dataDir,"/lidarTilesGTiff_5m/")





tile2m <- list.files(path = raster_dir2, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile3m <- list.files(path = raster_dir3, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile4m <- list.files(path = raster_dir4, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile10m <- list.files(path = raster_dir10, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile20m <- list.files(path = raster_dir20, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile50m <- list.files(path = raster_dir50, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)

tile1m <- list.files(path = raster_dir1, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)
tile5m <- list.files(path = raster_dir5, pattern = "ahn_140000_457000.tif", full.names = T, recursive = T)


r1<-raster(tile1m)
r2<-raster(tile2m)
r3<-raster(tile3m)
r4<-raster(tile4m)
r5<-raster(tile5m)
r10<-raster(tile10m)
r20<-raster(tile20m)
r50<-raster(tile50m)




MBDeBilt1Cell<-microbenchmark(
  svf100_16_1m = svf(r1, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_2m = svf(r2, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_3m = svf(r3, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_4m = svf(r4, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_5m = svf(r5, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_10m = svf(r10, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_20m = svf(r20, nAngles = 16, maxDist = 100, ll=F),
  svf100_16_50m = svf(r50, nAngles = 16, maxDist = 100, ll=F),
  
  times = 10
)

print(MBDeBilt1Cell)
