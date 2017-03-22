library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(uuid)
library(data.table)
library(stringr)

#All the functions are stored in the folder functions
#With the library R.utils and the command sourceDirectory all the functions are loaded
library(R.utils)
sourceDirectory("functions")

#####################################################################
#DIRECTORIES
#####################################################################
output_dir<-"/home/pagani/development/SkyViewFactor/data/gridsSVF/"
lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
#####################################################################

#####################################################################
#INITIAL SETTINGS
#####################################################################
workingPath <<- getwd()
#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

Xres<<-5 # x-resolution in meters
Yres<<-5 # y-resolution in meters

maxView<<-100 # max view for the SVF

registerDoParallel(8) #number of parallel cores
#####################################################################


#####################################################################
#LOAD A POINT DATA-SET
#####################################################################
GMS_meta<-fread("GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)
#####################################################################




main<-function(){
coordsGMS<-as(GMS_meta,"SpatialPoints")
coordsGMS<-data.frame(coordsGMS)

coordsGMS$tileNumberXCoord<-floor(coordsGMS$loc_lon/1000)*1000
coordsGMS$tileNumberYCoord<-floor(coordsGMS$loc_lat/1000)*1000

#tiles_unique<-unique(coordsGMS[c("tileNumberXCoord","tileNumberYCoord")])
dir.create("/home/pagani/development/SkyViewFactor/data/tiles")

system.time(
foreach(i =  1:length(coordsGMS[,1]), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"), 
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "maxView", "Xres", "Yres", "coord")) %dopar%
{
  print(i)
    if(!file.exists(paste0(workingPath,"/data/gridsSVF/",
                           str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
                           str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri")))
      {
      #print("ABC")
    print(paste0(workingPath,"/data/gridsSVF/",
                 str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
                 str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri"))
    SVF(coordsGMS[i,]$loc_lon, coordsGMS[i,]$loc_lat,maxView, pro)
    gc()
  }
})
}




