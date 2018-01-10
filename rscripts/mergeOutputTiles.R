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
library(parallel)


output_dir<<-"/home/pagani/SVFAmsterdam"
  #"/home/ubuntu/efs/output/SVF_1m/"


files<-list.files(output_dir,full.names = T, pattern = "*.gri")


rasterOutput<-function(x){
  ras<-brick(x)
  rr<-ras[[2]]
}

cl<-makeCluster(16, type = "FORK")



wholeRasterList<-parLapply(cl,files,rasterOutput)
stopCluster(cl)
#rasterOptions(tolerance = 100)
#options(overlap=F)
totalRaster<-do.call(merge, c(wholeRasterList, list(tolerance=100)))
sections<-splitRaster(totalRaster,2,2,path = "~/temp/splits/")

