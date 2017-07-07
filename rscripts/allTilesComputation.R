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
output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF_200m/"
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

maxView<<-200 # max view for the SVF

registerDoParallel(10) #number of parallel cores
#####################################################################



main<-function(){

listTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)

#listTiles <- listTiles[40001:length(listTiles)]


#SVF(tiles_unique[1,]$tileNumberXCoord, tiles_unique[1,]$tileNumberYCoord,maxView, pro)

logfile<-"logNew200m.txt"

system.time(
foreach(i =  1:length(listTiles), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "temp_dir", "maxView", "Xres", "Yres",
                    "loadNeighborTile_v2","mergeNeighborTiles"), .combine = f(length(listTiles))) %dopar%
{
  write(paste("processing ", i, listTiles[[i]]),file = logfile, append = T)
  outp<-1
  tilesToBeWorked<-getTileNumber(listTiles[[i]])
    if(!file.exists(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri")))
     {

      
      #print("ABC")
    # print(paste0(output_dir,
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri"))
      tryCatch(outp<-SVFWholeNL(listTiles[[i]],maxView), error=function(e){print(paste0("tile with point x=", listTiles[[i]][1], " y=",listTiles[[i]][2]," not available in dataset. Skipping point.")); return(NULL)})

      #SVFWholeNL(listTiles[[i]],maxView)

      #tryCatch(outp<-SVF(coord[[i]][1], coord[[i]][2],maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
      if(is.null(outp))
      {
        print(paste("error in tile ",listTiles[[i]]))
        next
      }
      else if (outp == -1)
      {
        write(paste("tile has no neighbors",listTiles[[i]]),logfile, append = T)
        
      }
    gc()
    }
  else {
   # print(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri", " already computed"))
  }
})

unlink(temp_dir, recursive = T)
}


getTileNumber <- function(filepath){
  
  
  file<-basename(filepath)
  splits<-unlist(strsplit(file, c("\\.")))
  splits<-unlist(strsplit(splits[[1]], "_"))
  
  
  coordX<-splits[[2]] #str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-splits[[3]]
  
  coordsXY<-c(str_pad(as.integer(coordX), width = 6, pad = "0"), str_pad(as.integer(coordY), width = 6, pad = "0"))
  rm(file,splits,coordX,coordY)
  return(coordsXY)
  
}





#Progress combine function
f <- function(n){
  pb <- txtProgressBar(min=1, max=n-1,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    #Sys.sleep(5)
    flush.console()
    c(...)
  }
}



