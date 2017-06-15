#coordinates box
#Den Haag  51.7-52.4 N, 3.8-4.9 E. 
#Eindhoven 51.1-51.9 N, 5-6 E.
library(R.utils)
library(magrittr)
library(stringr)

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
library(sp)

sourceDirectory("~/development/SkyViewFactor/functions")

pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

Xres<<-5 # x-resolution in meters
Yres<<-5 # y-resolution in meters

maxView<<-100 # max view for the SVF


checkNonComputedTiles<-function(){


Haag<-extent(3.8,4.9,51.7,52.4)
Eindhoven<-extent(5,6,51.1,51.9)

Haag.ext<-extent_from_new_pro(Haag)
Eindhoven.ext<-extent_from_new_pro(Eindhoven)

files.Haag<-grid_file_name_from_extent(Haag.ext)
files.Eindhoven<-grid_file_name_from_extent(Eindhoven.ext)

saveRDS(files.Haag,"data/ReinderHaag.rds")
saveRDS(files.Eindhoven,"data/ReinderEindhoven.rds")

HaagTiles<-readRDS("/home/pagani/development/SkyViewFactor/data/ReinderHaag.rds")
EindhovenTiles<-readRDS("/home/pagani/development/SkyViewFactor/data/ReinderEindhoven.rds")

#copy data with RCurl
#scp() #some code to copy


##Checking if the suppoosed GRD files are computed already or those tiles do not exist (e.g., sea, Germany, Belgium)
wholeNLSVFData<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"

#files<-lapply(HaagTiles,list.files, path = wholeNLSVFData, full.names = T)


#tilesAvailableHaag<-sum(unlist(lapply(files, function(x) !identical(x,character(0)))))





#get laz files and clean the filename ahn_ and _1,2... to compare with computed
lazFolder <- c("/data1/", "/data2/", "/data3")
listLAZTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)
test11<-listLAZTiles %>% str_replace("_[1,2,3,4,5,6].laz",".laz")
lazRaw<-lapply(test11, function(x) str_replace(str_replace(tail(unlist(str_split(x,patter = "/")),1),pattern = ".laz",""),"ahn_",""))
removedUndescoreTiles<-unique(lazRaw)



#clean extention of files that are in the Haag Eindoven extentions
rawTilesNamesHaagToHave<-HaagTiles %>% str_replace(".grd", "")
rawTilesNamesEindhovenToHave<-EindhovenTiles %>% str_replace(".grd", "")



#number of tiles to be processed for Haag Eindhoven that overlap with Laz (sea, Belgium, Germany are not needed)
totalLazForHaag<-sum(rawTilesNamesHaagToHave %in% removedUndescoreTiles)
totalLazForEindhoven<-sum(rawTilesNamesEindhovenToHave %in% removedUndescoreTiles)
lazToBeProcessedForHaag<-rawTilesNamesHaagToHave[rawTilesNamesHaagToHave %in% removedUndescoreTiles]
lazToBeProcessedForEindhoven<-rawTilesNamesEindhovenToHave[rawTilesNamesEindhovenToHave %in% removedUndescoreTiles]


#get the files that are already computed
computedFiles<-list.files(wholeNLSVFData, ".grd")
#computedAvailableForHaag<-sum(HaagTiles %in% computedFiles)
#computedAvailableForEindhoven<-sum(EindhovenTiles %in% computedFiles)
computedFiles<-computedFiles %>% str_replace(".grd", "")


#compare the computed with the one to be computed
stillToComputeEindhoven<-lazToBeProcessedForEindhoven[!lazToBeProcessedForEindhoven %in% computedFiles]
stillToComputeHaag<-lazToBeProcessedForHaag[!lazToBeProcessedForHaag %in% computedFiles]

length(stillToComputeEindhoven)

return(stillToComputeEindhoven)
}

##SAVING THE FILES TO A ZIP
#if(length(stillToComputeHaag)==0){
#readyToShipHaagGRI<-unlist(lapply(lazToBeProcessedForHaag[lazToBeProcessedForHaag %in% computedFiles], paste0, ".grd"))
#readyToShipHaagGRD<-unlist(lapply(lazToBeProcessedForHaag[lazToBeProcessedForHaag %in% computedFiles], paste0, ".gri"))
#setwd("/home/pagani/development/SkyViewFactor/data/gridsNLSVF/")
#files<-c(readyToShipHaagGRD, readyToShipHaagGRI)
#file.copy(files, "~/filesReiner/")
#Zipping directly from R somehow fails
#zip("/home/pagani/archiveSVFHaag.zip", files = files)
#}

if(length(stillToComputeEindhoven)==0){
readyToShipEindhovenGRI<-unlist(lapply(lazToBeProcessedForEindhoven[lazToBeProcessedForEindhoven %in% computedFiles], paste0, ".grd"))
readyToShipEindhovenGRD<-unlist(lapply(lazToBeProcessedForEindhoven[lazToBeProcessedForEindhoven %in% computedFiles], paste0, ".gri"))
setwd("/home/pagani/development/SkyViewFactor/data/gridsNLSVF/")
files<-c(readyToShipEindhovenGRD, readyToShipEindhovenGRI)
file.copy(files, "~/SVFEindhoven/")
#Zipping directly from R somehow fails
#zip("/home/pagani/archiveSVFHaag.zip", files = files)
}




##########TILES EINDHOVEN NOT ACCORDING TO NAMING CONVENTION############################
#leftRAWEIND<-rawTilesNamesEindhovenToHave[rawTilesNamesEindhovenToHave %in% computedFiles]
#leftLAZEIND<-lazToBeProcessedForEindhoven[lazToBeProcessedForEindhoven %in% computedFiles]
#leftRAWEIND %in% leftLAZEIND
#leftRAWEIND[!leftRAWEIND %in% leftLAZEIND]
#####################################
 
# if(computedAvailableForHaag==totalLazForHaag){
#   message("Haag is fully computed")
# } else{
#   message("Haag not fully computed")
# }
# 
# if(computedAvailableForEindhoven==totalLazForEindhoven){
#   message("Eindhoven is fully computed")
# } else{
#   message("Einhoven not fully computed")
# }




computeTilesReinder<-function(tilesList){
 
  
  output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"
  lazFolder <<- c("/data1/", "/data2/", "/data3")
  lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
  dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
  temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"
  
  
  
  coords<-lapply(tilesList, str_split, "_")
  # pro<-CRS("+init=epsg:28992")
  # WGS84<-CRS("+init=epsg:4326")
  # 
  # Xres<-5 # x-resolution in meters
  # Yres<-5 # y-resolution in meters
  # 
  # maxView<-100 # max view for the SVF
  
  tiles_unique<-data.frame(matrix(as.numeric(unlist(coords)), nrow = length(coords), byrow = T))
  colnames(tiles_unique)<-c("tileNumberXCoord","tileNumberYCoord")
  
  registerDoParallel(10)
  
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
                      }
  
  
  
}


