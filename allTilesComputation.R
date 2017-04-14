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

#####################################################################
#DIRECTORIES
#####################################################################
workingPath <<- getwd()

#Andrea
output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"
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

registerDoParallel(8) #number of parallel cores
#####################################################################



main<-function(){

listTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)

listTiles <- listTiles[1:2]


#SVF(tiles_unique[1,]$tileNumberXCoord, tiles_unique[1,]$tileNumberYCoord,maxView, pro)



system.time(
foreach(i =  1:length(listTiles), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "temp_dir", "maxView", "Xres", "Yres",
                    "loadNeighborTile_v2","mergeNeighborTiles")) %do%
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

      SVFWholeNL(listTiles[[i]],maxView)

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




