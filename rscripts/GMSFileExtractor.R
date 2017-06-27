#get the files corresponding to the GMS
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
sourceDirectory("functions")

#####################################################################
#DIRECTORIES
#####################################################################
workingPath <<- getwd()

#Andrea
output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"
lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"

#Marieke
# output_dir<<-"/home/dirksen/SVF/gridsSVF/"
# lazFolder <<- c("/data1/", "/data2/", "/data3")
# lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
# dir.create("/home/dirksen/SVF/temp/")
# temp_dir<<-"/home/dirksen/SVF/temp/"

#####################################################################






#global vars/ config vars
pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

GMS_meta<-fread("/home/pagani/development/SkyViewFactor/GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)

coordsGMS<-as(GMS_meta,"SpatialPoints")
coordsGMS<-data.frame(coordsGMS)

coordsGMS$tileNumberXCoord<-floor(coordsGMS$loc_lon/1000)*1000
coordsGMS$tileNumberYCoord<-floor(coordsGMS$loc_lat/1000)*1000

tiles_unique<-unique(coordsGMS[c("tileNumberXCoord","tileNumberYCoord")])

tiles_unique100 <-tiles_unique[1:100,]

mc<-20

cl<-makeCluster(mc)

filesToCopy<-NULL
clusterEvalQ(cl,expr = c(library(stringr), source("NeighborsFilesCopy.R"), library(R.utils)
, sourceDirectory("R")))
clusterExport(cl, varlist = c("filesToCopy", "lazFolder", "lasZipLocation"))


filesss<-parApply(cl=cl, X=tiles_unique100, MARGIN = 1, FUN= function(w) {

x<-w[1]#$tileNumberXCoord
y<-w[2]#$tileNumberYCoord

x<-str_pad(as.integer(floor(x/1000)*1000), 6, pad = "0")
y<-str_pad(as.integer(floor(y/1000)*1000), 6, pad = "0")


# multifileFlag<-checkIfMultiTile(lazFolder, x, y)
# if(multifileFlag){
#   file_s<-list.files(path = lazFolder, pattern = paste0("ahn_", x,"_", y,"_.*"), full.names = T, recursive = T)
# } else {
#   file_s<-list.files(path = lazFolder, pattern = paste0("ahn_", x,"_", y,".*"), full.names = T, recursive = T)
# }


filesToCopy<-c(filesToCopy,getNeighborAndMainTilesFile(lazFolder, x, y))

})

files2<-unlist(filesss)
parLapply(cl, files2, file.copy, to = "/ssd1/GMSsvf/")

filesZipped<-list.files(path = "/ssd1/GMSsvf", pattern = paste0("*.laz"), full.names = T, recursive = T)

parLapply(cl, X = filesZipped, fun = function(x) { system(paste(lasZipLocation, x))})















