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

#All the functions are stored in the folder functions
#With the library R.utils and the command sourceDirectory all the functions are loaded
library(R.utils)
sourceDirectory("functions")

#Newest function
testTile<-"/ssd1/GMSsvf/ahn_261000_468000.las"
testPoint<-readRDS("test_data/point.rds")
test<-lines_from_las()
#####################################################################
#DIRECTORIES
#####################################################################
workingPath <<- getwd()

#Andrea
output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsSVF/"
lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"

#Marieke
# output_dir<<-"/home/dirksen/SVF/gridsSVF/"
# lazFolder <<- c("/data1/", "/data2/", "/data3")
# lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
# dir.create("/home/dirksen/SVF/temp/")
# temp_dir<<-"/home/dirksen/SVF/temp/"

#####################################################################

#####################################################################
#INITIAL SETTINGS
#####################################################################
#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

Xres<<-5 # x-resolution in meters
Yres<<-5 # y-resolution in meters

maxView<<-100 # max view for the SVF

registerDoParallel(11) #number of parallel cores
#####################################################################


#####################################################################
#LOAD A POINT DATA-SET
#####################################################################
GMS_meta<-fread("/home/pagani/development/SkyViewFactor/GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)
#####################################################################

mainWithCoords<-function(){
coordsGMS<-as(GMS_meta,"SpatialPoints")
coordsGMS<-data.frame(coordsGMS)

coordsGMS$tileNumberXCoord<-floor(coordsGMS$loc_lon/1000)*1000
coordsGMS$tileNumberYCoord<-floor(coordsGMS$loc_lat/1000)*1000

tiles_unique<-unique(coordsGMS[c("tileNumberXCoord","tileNumberYCoord")])

#tiles_unique<<-tiles_unique[1:5,]



#SVF(tiles_unique[1,]$tileNumberXCoord, tiles_unique[1,]$tileNumberYCoord,maxView, pro)



system.time(
foreach(i =  1:length(tiles_unique[,1]), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "temp_dir", "maxView", "Xres", "Yres",
                    "loadNeighborTile_v2","mergeNeighborTiles")) %dopar%
{
  print(i)
  outp<-1
    if(!file.exists(paste0(output_dir,
                           str_pad(as.integer(tiles_unique[i,]$tileNumberXCoord), width = 6, pad = "0"),"_",
                           str_pad(as.integer(tiles_unique[i,]$tileNumberYCoord),  width = 6, pad = "0"), ".gri")))
      {

      print(paste0(output_dir,
                   str_pad(as.integer(tiles_unique[i,]$tileNumberXCoord), width = 6, pad = "0"),"_",
                   str_pad(as.integer(tiles_unique[i,]$tileNumberYCoord),  width = 6, pad = "0"), ".gri"))
      #print("ABC")
    # print(paste0(output_dir,
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri"))
      #tryCatch(outp<-SVF(tiles_unique[i,]$tileNumberXCoord, tiles_unique[i,]$tileNumberYCoord,maxView, pro), error=function(e){print(paste0("tile with point x=", tiles_unique[[i]][1], " y=",tiles_unique[[i]][2]," not available in dataset. Skipping point.")); return(NULL)})

      SVF(tiles_unique[i,]$tileNumberXCoord, tiles_unique[i,]$tileNumberYCoord,maxView, pro)

      #tryCatch(outp<-SVF(coord[[i]][1], coord[[i]][2],maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
      if(is.null(outp))
      {
        next
      }
    gc()
  }
})

unlink(temp_dir, recursive = T)
}




