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
library(SpaDES)

output_dir<<-"/home/ubuntu/efs/output/SVF_1m/"
splits_dir<<-"/home/ubuntu/efs/output/SVF_1m_regionsNew/"


files<-list.files(output_dir,full.names = T, pattern = "*.gri")


rasterOutput<-function(x){
  ras<-brick(x)
  rr<-ras[[2]]
  rr
}



mergeAndSplit<-function(files){
rasterOptions(maxmemory=58e9)
message("building cluster")
cl<-makeCluster(12, type = "FORK")


message("select the layer and getting the raster from every tile")
wholeRasterList<-parLapply(cl,files,rasterOutput)
stopCluster(cl)

#rasterOptions(tolerance = 100)
#options(overlap=F)

message("starting merging the tiles")
totalRaster<-do.call(merge, c(wholeRasterList, list(tolerance=100)))
message("full raster merged, now split in 16 regions")
#totalRaster<-raster("/home/ubuntu/efs/output/tempRastersCopy/r_tmp_2018-01-10_213252_2546_27069.grd")
sections<-splitRaster(totalRaster,4,4,path =splits_dir)

}


removeArtifacts<-function(rr){
#to check if there are artifacts where SVF<0
#to be corrected with
s<-calc(rr, fun=function(x){x[x<0]<-0; return(x)})
s
}

convertToNetCDF<-function(file){
  ras<-raster(file)
  ras<-removeArtifacts(ras)
  writeRaster(ras,gsub(".gri",".nc",file))
}




rasterOptions(maxmemory=20e9)
cl<-makeCluster(12, type = "FORK")
parLapply(cl, files,convertToNetCDF)
stopCluster()



