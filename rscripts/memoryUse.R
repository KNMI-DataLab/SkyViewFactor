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


library(R.utils)


lazFolder <- c("/data1/", "/data2/", "/data3")


sourceDirectory("functions")

set.seed(11)

listTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)


registerDoParallel(15)

toProcess<-sample(listTiles,200)

memory<-foreach(i = 1:length(toProcess), .combine = c, .packages = c("raster", "horizon", "rgdal", "rLiDAR")) %dopar% {
  currentFile<-toProcess[[i]]
  file.copy(currentFile, "/home/pagani/development/SkyViewFactor/data/LAZsample/")
  fileLaz<-tail(strsplit(currentFile, "/")[[1]], 1)
  rootFile<-strsplit(fileLaz, "\\.")[[1]][[1]]
  system(paste0("/home/pagani/tools/LAStools/bin/laszip", " /home/pagani/development/SkyViewFactor/data/LAZsample/", fileLaz))
  lasFile<-paste0(rootFile,".las")
  lasData<-readLAS(paste0("/home/pagani/development/SkyViewFactor/data/LAZsample/",lasFile))
  DF<-data.frame(lasData)
  pro<-CRS("+init=epsg:28992")
  DFSpatial<-makeSpatialDF(DF,projection = pro)
  Xres<-1
  Yres<-1
  DFraster<-makeRaster(DFSpatial,Xres,Yres,pro)
  system(paste0("rm ", "/home/pagani/development/SkyViewFactor/data/LAZsample/",lasFile))
  system(paste0("rm ", "/home/pagani/development/SkyViewFactor/data/LAZsample/",fileLaz))
  rm(lasData,DF,DFSpatial)
  usageMem<-object.size(DFraster)
  usageMem
  
}

print(memory)





