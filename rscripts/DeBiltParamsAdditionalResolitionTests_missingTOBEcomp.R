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
#library(uuid)
library(data.table)
library(stringr)
library(spatial.tools)
library(sp)

library(parallel)
library(microbenchmark)


sourceDirectory("~/development/SkyViewFactor/functions")






prepareTiles<-function()
{
lasZipLocation <- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
temp_dir <- "/home/pagani/development/SkyViewFactor/data/DeBiltSample/"




pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")



maxView<<-100 # max view for the SVF

output_dir<-"/data1/lidarTilesDeBilt5m/"
registerDoParallel(3) #number of parallel cores


#checkNonComputedTiles<-function(){
  
  
  DeBilt<-extent(5.12907,5.23293,52.07117,52.12431)
  #Eindhoven<-extent(5,6,51.1,51.9)
  
  DeBilt.ext<-extent_from_new_pro(DeBilt)
  DeBilt.ext.margin<-DeBilt.ext+2000 #add a safety margin to pick the neighbor tiles to allow for the radius

  files.DeBilt<-grid_file_name_from_extent(DeBilt.ext.margin)

  saveRDS(files.DeBilt,"data/DeBiltTiles.rds")

  DeBiltTiles<-readRDS("/home/pagani/development/SkyViewFactor/data/DeBiltTiles.rds")

  #copy data with RCurl
  #scp() #some code to copy
  
  
  ##Checking if the suppoosed GRD files are computed already or those tiles do not exist (e.g., sea, Germany, Belgium)
  wholeNLSVFData<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"
  
  #files<-lapply(HaagTiles,list.files, path = wholeNLSVFData, full.names = T)
  
  
  #tilesAvailableHaag<-sum(unlist(lapply(files, function(x) !identical(x,character(0)))))
  
  
  DeBiltTiles<-lapply(DeBiltTiles, function(x) str_replace(x, ".grd", ".laz"))
  DeBiltTiles<-paste0("ahn_",DeBiltTiles)
  
  
  
  #get laz files and clean the filename ahn_ and _1,2... to compare with computed
  lazFolder <- c("/data1/", "/data2/", "/data3")
  listLAZTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)
  
  
  
  #test11<-listLAZTiles %>% str_replace("_[1,2,3,4,5,6].laz",".laz")
  #lazRaw<-lapply(test11, function(x) str_replace(str_replace(tail(unlist(str_split(x,patter = "/")),1),pattern = ".laz",""),"ahn_",""))
  #removedUndescoreTiles<-unique(lazRaw)
  
  
  
  #clean extention of files that are in the Haag Eindoven extentions
  #rawTilesNamesDeBiltToHave<-DeBiltTiles %>% str_replace(".grd", "")

  
  
  #number of tiles to be processed for Haag Eindhoven that overlap with Laz (sea, Belgium, Germany are not needed)
  #totalLazForDeBilt<-sum(rawTilesNamesDeBiltToHave %in% removedUndescoreTiles)
  lazToBeProcessedForDeBilt<-listLAZTiles[basename(listLAZTiles) %in% DeBiltTiles]

  
  
  
  
  
  
  foreach(i =  1:length(lazToBeProcessedForDeBilt), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
          .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                      "pro", "workingPath", "lazFolder", "lasZipLocation", "temp_dir", "maxView", "Xres", "Yres",
                      "loadNeighborTile_v2","mergeNeighborTiles")) %dopar%
                      {
                        fileLAZ<-lazToBeProcessedForDeBilt[[i]]
                        splitStr<-stringr::str_split(fileLAZ,"/", simplify = T)
                        fileLAS<-splitStr[[length(splitStr)]]
                        filename<-str_split(fileLAS,"\\.", simplify = T)[[1]]
                        fileLAS<-paste0(temp_dir,filename,".las")
                        
                        
                        if(!file.exists(paste0(output_dir, filename, ".grd")))
                        {
                          system(paste0(lasZipLocation, " -i ", fileLAZ, " -o ", fileLAS))
                          lasData<-readLAS(fileLAS)
                          file.remove(fileLAS)
                          DF<-data.frame(lasData)
                          pro<-CRS("+init=epsg:28992")
                          DFSpatial<-makeSpatialDF(DF,projection = pro)
                          Xres<-5
                          Yres<-5
                          DFraster<-makeRaster(DFSpatial,Xres,Yres,pro)
                          #writeRaster(DFraster,"/home/pagani/development/SkyViewFactor/data/LAZsample/testRaster")
                          writeRaster(DFraster,paste0(output_dir,filename))
                          gc()
                        }
                        
                      }
  
  
}





raster_dir<-"/home/ubuntu/efs/DeBilt1m/"

raster_dir_bird<-"/data1/lidarTilesDeBilt1m/"



sensAnalisysOutput_dir_bird<-"/home/pagani/development/outputTilesTest/sensAnalysisHorizonV12/"


sensAnalysisOutput_dir<-"/home/ubuntu/efs/sensAnalysisHorizonV12/"



listGRDTiles <- list.files(path = raster_dir, pattern = ".grd", full.names = T, recursive = T)

#listGRDTiles<-list(listGRDTiles)
#commented out for the single test of the timing
test<-lapply(listGRDTiles,raster)
merged <- do.call(merge, test)


raster1m<-merged
raster2m<-aggregate(merged,fact=2)
raster3m<-aggregate(merged,fact=3)
raster4m<-aggregate(merged,fact=4)
raster5m<-aggregate(merged,fact=5)
raster10m<-aggregate(merged,fact=10)
raster20m<-aggregate(merged,fact=20)
raster50m<-aggregate(merged,fact=50)




tasks<-list(

  #1m
#   
#   svf_1m_1d_5r = function() {svf_1m_1d_5r<-svf(raster1m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_1m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_5r"))
#   },
#   svf_1m_1d_10r = function() {svf_1m_1d_10r<-svf(raster1m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_1m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_10r"))
#   },
#   svf_1m_1d_25r = function() { svf_1m_1d_25r <- svf(raster1m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_1m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_25r"))
#   },
#   svf_1m_1d_50r = function() {svf_1m_1d_50r<-svf(raster1m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_1m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_50r"))
#   },
#   svf_1m_1d_100r = function() {svf_1m_1d_100r<-svf(raster1m, nAngles = 1, maxDist = 100, ll=F)
#   writeRaster(svf_1m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_100r"))
#   },
#   svf_1m_1d_200r = function(){ svf_1m_1d_200r<-svf(raster1m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_1m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_200r"))
#   },
#   svf_1m_1d_400r = function() {svf_1m_1d_400r<-svf(raster1m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_1m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_400r"))
#   },
#   svf_1m_1d_800r = function() {svf_1m_1d_800r<-svf(raster1m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_1m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_1m_2d_5r = function() {svf_1m_2d_5r<-svf(raster1m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_1m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_5r"))
#   },
#   svf_1m_2d_10r = function() {svf_1m_2d_10r<-svf(raster1m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_1m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_10r"))
#   },
#   svf_1m_2d_25r = function() { svf_1m_2d_25r <- svf(raster1m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_1m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_25r"))
#   },
#   svf_1m_2d_50r = function() {svf_1m_2d_50r<-svf(raster1m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_1m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_50r"))
#   },
#   svf_1m_2d_100r = function() {svf_1m_2d_100r<-svf(raster1m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_1m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_100r"))
#   },
#   svf_1m_2d_200r = function(){ svf_1m_2d_200r<-svf(raster1m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_1m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_200r"))
#   },
#   svf_1m_2d_400r = function() {svf_1m_2d_400r<-svf(raster1m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_1m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_400r"))
#   },
   svf_1m_2d_800r = function() {svf_1m_2d_800r<-svf(raster1m, nAngles = 2, maxDist = 800, ll=F)
   writeRaster(svf_1m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_2d_800r"))
   },
#   
#   
#   
#   
#   svf_1m_3d_5r = function() {svf_1m_3d_5r<-svf(raster1m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_1m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_5r"))
#   },
#   svf_1m_3d_10r = function() {svf_1m_3d_10r<-svf(raster1m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_1m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_10r"))
#   },
#   svf_1m_3d_25r = function() { svf_1m_3d_25r <- svf(raster1m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_1m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_25r"))
#   },
#   svf_1m_3d_50r = function() {svf_1m_3d_50r<-svf(raster1m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_1m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_50r"))
#   },
   svf_1m_3d_100r = function() {svf_1m_3d_100r<-svf(raster1m, nAngles = 3, maxDist = 100, ll=F)
   writeRaster(svf_1m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_100r"))
   },
#   svf_1m_3d_200r = function(){ svf_1m_3d_200r<-svf(raster1m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_1m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_200r"))
#   },
#   svf_1m_3d_400r = function() {svf_1m_3d_400r<-svf(raster1m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_1m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_400r"))
#   },
#   svf_1m_3d_800r = function() {svf_1m_3d_800r<-svf(raster1m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_1m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_1m_4d_5r = function() {svf_1m_4d_5r<-svf(raster1m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_1m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_5r"))
#   },
#   svf_1m_4d_10r = function() {svf_1m_4d_10r<-svf(raster1m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_1m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_10r"))
#   },
#   svf_1m_4d_25r = function() { svf_1m_4d_25r <- svf(raster1m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_1m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_25r"))
#   },
#   svf_1m_4d_50r = function() {svf_1m_4d_50r<-svf(raster1m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_1m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_50r"))
#   },
#   svf_1m_4d_100r = function() {svf_1m_4d_100r<-svf(raster1m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_1m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_100r"))
#   },
#   svf_1m_4d_200r = function(){ svf_1m_4d_200r<-svf(raster1m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_1m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_200r"))
#   },
#   svf_1m_4d_400r = function() {svf_1m_4d_400r<-svf(raster1m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_1m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_400r"))
#   },
#   svf_1m_4d_800r = function() {svf_1m_4d_800r<-svf(raster1m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_1m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_4d_800r"))
#   },
#   
#   
#   
#   svf_1m_8d_5r = function() {svf_1m_8d_5r<-svf(raster1m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_1m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_5r"))
#   },
#   svf_1m_8d_10r = function() {svf_1m_8d_10r<-svf(raster1m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_1m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_10r"))
#   },
#   svf_1m_8d_25r = function() { svf_1m_8d_25r <- svf(raster1m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_1m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_25r"))
#   },
#   svf_1m_8d_50r = function() {svf_1m_8d_50r<-svf(raster1m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_1m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_50r"))
#   },
#   svf_1m_8d_100r = function() {svf_1m_8d_100r<-svf(raster1m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_1m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_100r"))
#   },
#   svf_1m_8d_200r = function(){ svf_1m_8d_200r<-svf(raster1m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_1m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_200r"))
#   },
#   svf_1m_8d_400r = function() {svf_1m_8d_400r<-svf(raster1m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_1m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_400r"))
#   },
#   svf_1m_8d_800r = function() {svf_1m_8d_800r<-svf(raster1m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_1m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_1m_16d_5r = function() {svf_1m_16d_5r<-svf(raster1m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_1m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_5r"))
#   },
#   svf_1m_16d_10r = function() {svf_1m_16d_10r<-svf(raster1m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_1m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_10r"))
#   },
#   svf_1m_16d_25r = function() { svf_1m_16d_25r <- svf(raster1m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_1m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_25r"))
#   },
#   svf_1m_16d_50r = function() {svf_1m_16d_50r<-svf(raster1m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_1m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_50r"))
#   },
#   svf_1m_16d_100r = function() {svf_1m_16d_100r<-svf(raster1m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_1m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_100r"))
#   },
#   svf_1m_16d_200r = function(){ svf_1m_16d_200r<-svf(raster1m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_1m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_200r"))
#   },
#   svf_1m_16d_400r = function() {svf_1m_16d_400r<-svf(raster1m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_1m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_400r"))
#   },
#   svf_1m_16d_800r = function() {svf_1m_16d_800r<-svf(raster1m, nAngles = 16, maxDist = 800, ll=F)
#   writeRaster(svf_1m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_16d_800r"))
#   },
#   
#   
#   
#   
#   svf_1m_32d_5r = function() {svf_1m_32d_5r<-svf(raster1m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_1m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_5r"))
#   },
#   svf_1m_32d_10r = function() {svf_1m_32d_10r<-svf(raster1m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_1m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_10r"))
#   },
#   svf_1m_32d_25r = function() { svf_1m_32d_25r <- svf(raster1m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_1m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_25r"))
#   },
#   svf_1m_32d_50r = function() {svf_1m_32d_50r<-svf(raster1m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_1m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_50r"))
#   },
#   svf_1m_32d_100r = function() {svf_1m_32d_100r<-svf(raster1m, nAngles = 32, maxDist = 100, ll=F)
#   writeRaster(svf_1m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_100r"))
#   },
#   svf_1m_32d_200r = function(){ svf_1m_32d_200r<-svf(raster1m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_1m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_200r"))
#   },
#   svf_1m_32d_400r = function() {svf_1m_32d_400r<-svf(raster1m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_1m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_400r"))
#   },
#   svf_1m_32d_800r = function() {svf_1m_32d_800r<-svf(raster1m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_1m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_1m_64d_5r = function() {svf_1m_64d_5r<-svf(raster1m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_1m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_5r"))
#   },
#   svf_1m_64d_10r = function() {svf_1m_64d_10r<-svf(raster1m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_1m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_10r"))
#   },
#   svf_1m_64d_25r = function() { svf_1m_64d_25r <- svf(raster1m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_1m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_25r"))
#   },
#   svf_1m_64d_50r = function() {svf_1m_64d_50r<-svf(raster1m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_1m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_50r"))
#   },
#   svf_1m_64d_100r = function() {svf_1m_64d_100r<-svf(raster1m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_1m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_100r"))
#   },
#   svf_1m_64d_200r = function(){ svf_1m_64d_200r<-svf(raster1m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_1m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_200r"))
#   },
#   svf_1m_64d_400r = function() {svf_1m_64d_400r<-svf(raster1m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_1m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_400r"))
#   },
   svf_1m_64d_800r = function() {svf_1m_64d_800r<-svf(raster1m, nAngles = 64, maxDist = 800, ll=F)
   writeRaster(svf_1m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_1m_64d_800r"))
   },
#   
#   
#   
#   
#   
#   
# 
# 
# # ###2m
# # 
# # 
#   svf_2m_1d_5r = function() {svf_2m_1d_5r<-svf(raster2m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_2m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_5r"))
#   },
#   svf_2m_1d_10r = function() {svf_2m_1d_10r<-svf(raster2m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_2m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_10r"))
#   },
#   svf_2m_1d_25r = function() { svf_2m_1d_25r <- svf(raster2m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_2m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_25r"))
#   },
#   svf_2m_1d_50r = function() {svf_2m_1d_50r<-svf(raster2m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_2m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_50r"))
#   },
   svf_2m_1d_100r = function() {svf_2m_1d_100r<-svf(raster2m, nAngles = 1, maxDist = 100, ll=F)
   writeRaster(svf_2m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_100r"))
   },
#   svf_2m_1d_200r = function(){ svf_2m_1d_200r<-svf(raster2m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_2m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_200r"))
#   },
#   svf_2m_1d_400r = function() {svf_2m_1d_400r<-svf(raster2m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_2m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_400r"))
#   },
#   svf_2m_1d_800r = function() {svf_2m_1d_800r<-svf(raster2m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_2m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_2m_2d_5r = function() {svf_2m_2d_5r<-svf(raster2m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_2m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_5r"))
#   },
#   svf_2m_2d_10r = function() {svf_2m_2d_10r<-svf(raster2m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_2m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_10r"))
#   },
#   svf_2m_2d_25r = function() { svf_2m_2d_25r <- svf(raster2m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_2m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_25r"))
#   },
#   svf_2m_2d_50r = function() {svf_2m_2d_50r<-svf(raster2m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_2m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_50r"))
#   },
#   svf_2m_2d_100r = function() {svf_2m_2d_100r<-svf(raster2m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_2m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_100r"))
#   },
#   svf_2m_2d_200r = function(){ svf_2m_2d_200r<-svf(raster2m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_2m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_200r"))
#   },
#   svf_2m_2d_400r = function() {svf_2m_2d_400r<-svf(raster2m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_2m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_400r"))
#   },
#   svf_2m_2d_800r = function() {svf_2m_2d_800r<-svf(raster2m, nAngles = 2, maxDist = 800, ll=F)
#   writeRaster(svf_2m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_2d_800r"))
#   },
#   
#   
#   
#   
#   svf_2m_3d_5r = function() {svf_2m_3d_5r<-svf(raster2m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_2m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_5r"))
#   },
#   svf_2m_3d_10r = function() {svf_2m_3d_10r<-svf(raster2m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_2m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_10r"))
#   },
#   svf_2m_3d_25r = function() { svf_2m_3d_25r <- svf(raster2m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_2m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_25r"))
#   },
#   svf_2m_3d_50r = function() {svf_2m_3d_50r<-svf(raster2m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_2m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_50r"))
#   },
#   svf_2m_3d_100r = function() {svf_2m_3d_100r<-svf(raster2m, nAngles = 3, maxDist = 100, ll=F)
#   writeRaster(svf_2m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_100r"))
#   },
#   svf_2m_3d_200r = function(){ svf_2m_3d_200r<-svf(raster2m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_2m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_200r"))
#   },
#   svf_2m_3d_400r = function() {svf_2m_3d_400r<-svf(raster2m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_2m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_400r"))
#   },
#   svf_2m_3d_800r = function() {svf_2m_3d_800r<-svf(raster2m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_2m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_2m_4d_5r = function() {svf_2m_4d_5r<-svf(raster2m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_2m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_5r"))
#   },
#   svf_2m_4d_10r = function() {svf_2m_4d_10r<-svf(raster2m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_2m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_10r"))
#   },
#   svf_2m_4d_25r = function() { svf_2m_4d_25r <- svf(raster2m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_2m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_25r"))
#   },
#   svf_2m_4d_50r = function() {svf_2m_4d_50r<-svf(raster2m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_2m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_50r"))
#   },
#   svf_2m_4d_100r = function() {svf_2m_4d_100r<-svf(raster2m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_2m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_100r"))
#   },
#   svf_2m_4d_200r = function(){ svf_2m_4d_200r<-svf(raster2m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_2m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_200r"))
#   },
#   svf_2m_4d_400r = function() {svf_2m_4d_400r<-svf(raster2m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_2m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_400r"))
#   },
#   svf_2m_4d_800r = function() {svf_2m_4d_800r<-svf(raster2m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_2m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_4d_800r"))
#   },
#   
#   
#   
#   svf_2m_8d_5r = function() {svf_2m_8d_5r<-svf(raster2m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_2m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_5r"))
#   },
#   svf_2m_8d_10r = function() {svf_2m_8d_10r<-svf(raster2m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_2m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_10r"))
#   },
#   svf_2m_8d_25r = function() { svf_2m_8d_25r <- svf(raster2m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_2m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_25r"))
#   },
#   svf_2m_8d_50r = function() {svf_2m_8d_50r<-svf(raster2m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_2m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_50r"))
#   },
#   svf_2m_8d_100r = function() {svf_2m_8d_100r<-svf(raster2m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_2m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_100r"))
#   },
#   svf_2m_8d_200r = function(){ svf_2m_8d_200r<-svf(raster2m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_2m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_200r"))
#   },
#   svf_2m_8d_400r = function() {svf_2m_8d_400r<-svf(raster2m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_2m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_400r"))
#   },
#   svf_2m_8d_800r = function() {svf_2m_8d_800r<-svf(raster2m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_2m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_2m_16d_5r = function() {svf_2m_16d_5r<-svf(raster2m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_2m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_5r"))
#   },
#   svf_2m_16d_10r = function() {svf_2m_16d_10r<-svf(raster2m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_2m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_10r"))
#   },
#   svf_2m_16d_25r = function() { svf_2m_16d_25r <- svf(raster2m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_2m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_25r"))
#   },
#   svf_2m_16d_50r = function() {svf_2m_16d_50r<-svf(raster2m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_2m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_50r"))
#   },
#   svf_2m_16d_100r = function() {svf_2m_16d_100r<-svf(raster2m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_2m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_100r"))
#   },
#   svf_2m_16d_200r = function(){ svf_2m_16d_200r<-svf(raster2m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_2m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_200r"))
#   },
#   svf_2m_16d_400r = function() {svf_2m_16d_400r<-svf(raster2m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_2m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_400r"))
#   },
   svf_2m_16d_800r = function() {svf_2m_16d_800r<-svf(raster2m, nAngles = 16, maxDist = 800, ll=F)
   writeRaster(svf_2m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_16d_800r"))
   },
#   
#   
#   
#   
#   svf_2m_32d_5r = function() {svf_2m_32d_5r<-svf(raster2m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_2m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_5r"))
#   },
#   svf_2m_32d_10r = function() {svf_2m_32d_10r<-svf(raster2m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_2m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_10r"))
#   },
#   svf_2m_32d_25r = function() { svf_2m_32d_25r <- svf(raster2m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_2m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_25r"))
#   },
#   svf_2m_32d_50r = function() {svf_2m_32d_50r<-svf(raster2m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_2m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_50r"))
#   },
   svf_2m_32d_100r = function() {svf_2m_32d_100r<-svf(raster2m, nAngles = 32, maxDist = 100, ll=F)
   writeRaster(svf_2m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_100r"))
   },
#   svf_2m_32d_200r = function(){ svf_2m_32d_200r<-svf(raster2m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_2m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_200r"))
#   },
#   svf_2m_32d_400r = function() {svf_2m_32d_400r<-svf(raster2m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_2m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_400r"))
#   },
#   svf_2m_32d_800r = function() {svf_2m_32d_800r<-svf(raster2m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_2m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_2m_64d_5r = function() {svf_2m_64d_5r<-svf(raster2m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_2m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_5r"))
#   },
#   svf_2m_64d_10r = function() {svf_2m_64d_10r<-svf(raster2m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_2m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_10r"))
#   },
#   svf_2m_64d_25r = function() { svf_2m_64d_25r <- svf(raster2m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_2m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_25r"))
#   },
#   svf_2m_64d_50r = function() {svf_2m_64d_50r<-svf(raster2m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_2m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_50r"))
#   },
#   svf_2m_64d_100r = function() {svf_2m_64d_100r<-svf(raster2m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_2m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_100r"))
#   },
#   svf_2m_64d_200r = function(){ svf_2m_64d_200r<-svf(raster2m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_2m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_200r"))
#   },
#   svf_2m_64d_400r = function() {svf_2m_64d_400r<-svf(raster2m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_2m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_400r"))
#   },
#   svf_2m_64d_800r = function() {svf_2m_64d_800r<-svf(raster2m, nAngles = 64, maxDist = 800, ll=F)
#   writeRaster(svf_2m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_2m_64d_800r"))
#   },
#   
#   
# 
#   
#   
#   
# # 
# # 
# # ###3m
# # 
# 
#   svf_3m_1d_5r = function() {svf_3m_1d_5r<-svf(raster3m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_3m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_5r"))
#   },
#   svf_3m_1d_10r = function() {svf_3m_1d_10r<-svf(raster3m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_3m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_10r"))
#   },
#   svf_3m_1d_25r = function() { svf_3m_1d_25r <- svf(raster3m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_3m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_25r"))
#   },
#   svf_3m_1d_50r = function() {svf_3m_1d_50r<-svf(raster3m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_3m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_50r"))
#   },
#   svf_3m_1d_100r = function() {svf_3m_1d_100r<-svf(raster3m, nAngles = 1, maxDist = 100, ll=F)
#   writeRaster(svf_3m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_100r"))
#   },
#   svf_3m_1d_200r = function(){ svf_3m_1d_200r<-svf(raster3m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_3m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_200r"))
#   },
#   svf_3m_1d_400r = function() {svf_3m_1d_400r<-svf(raster3m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_3m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_400r"))
#   },
#   svf_3m_1d_800r = function() {svf_3m_1d_800r<-svf(raster3m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_3m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_3m_2d_5r = function() {svf_3m_2d_5r<-svf(raster3m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_3m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_5r"))
#   },
#   svf_3m_2d_10r = function() {svf_3m_2d_10r<-svf(raster3m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_3m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_10r"))
#   },
#   svf_3m_2d_25r = function() { svf_3m_2d_25r <- svf(raster3m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_3m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_25r"))
#   },
#   svf_3m_2d_50r = function() {svf_3m_2d_50r<-svf(raster3m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_3m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_50r"))
#   },
#   svf_3m_2d_100r = function() {svf_3m_2d_100r<-svf(raster3m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_3m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_100r"))
#   },
#   svf_3m_2d_200r = function(){ svf_3m_2d_200r<-svf(raster3m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_3m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_200r"))
#   },
#   svf_3m_2d_400r = function() {svf_3m_2d_400r<-svf(raster3m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_3m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_400r"))
#   },
#   svf_3m_2d_800r = function() {svf_3m_2d_800r<-svf(raster3m, nAngles = 2, maxDist = 800, ll=F)
#   writeRaster(svf_3m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_2d_800r"))
#   },
#   
#   
#   
#   
#   svf_3m_3d_5r = function() {svf_3m_3d_5r<-svf(raster3m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_3m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_5r"))
#   },
#   svf_3m_3d_10r = function() {svf_3m_3d_10r<-svf(raster3m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_3m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_10r"))
#   },
#   svf_3m_3d_25r = function() { svf_3m_3d_25r <- svf(raster3m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_3m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_25r"))
#   },
#   svf_3m_3d_50r = function() {svf_3m_3d_50r<-svf(raster3m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_3m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_50r"))
#   },
#   svf_3m_3d_100r = function() {svf_3m_3d_100r<-svf(raster3m, nAngles = 3, maxDist = 100, ll=F)
#   writeRaster(svf_3m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_100r"))
#   },
#   svf_3m_3d_200r = function(){ svf_3m_3d_200r<-svf(raster3m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_3m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_200r"))
#   },
#   svf_3m_3d_400r = function() {svf_3m_3d_400r<-svf(raster3m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_3m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_400r"))
#   },
#   svf_3m_3d_800r = function() {svf_3m_3d_800r<-svf(raster3m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_3m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_3m_4d_5r = function() {svf_3m_4d_5r<-svf(raster3m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_3m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_5r"))
#   },
#   svf_3m_4d_10r = function() {svf_3m_4d_10r<-svf(raster3m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_3m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_10r"))
#   },
#   svf_3m_4d_25r = function() { svf_3m_4d_25r <- svf(raster3m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_3m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_25r"))
#   },
#   svf_3m_4d_50r = function() {svf_3m_4d_50r<-svf(raster3m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_3m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_50r"))
#   },
#   svf_3m_4d_100r = function() {svf_3m_4d_100r<-svf(raster3m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_3m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_100r"))
#   },
#   svf_3m_4d_200r = function(){ svf_3m_4d_200r<-svf(raster3m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_3m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_200r"))
#   },
#   svf_3m_4d_400r = function() {svf_3m_4d_400r<-svf(raster3m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_3m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_400r"))
#   },
   svf_3m_4d_800r = function() {svf_3m_4d_800r<-svf(raster3m, nAngles = 4, maxDist = 800, ll=F)
   writeRaster(svf_3m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_4d_800r"))
   },
#   
#   
#   
#   svf_3m_8d_5r = function() {svf_3m_8d_5r<-svf(raster3m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_3m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_5r"))
#   },
#   svf_3m_8d_10r = function() {svf_3m_8d_10r<-svf(raster3m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_3m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_10r"))
#   },
#   svf_3m_8d_25r = function() { svf_3m_8d_25r <- svf(raster3m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_3m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_25r"))
#   },
#   svf_3m_8d_50r = function() {svf_3m_8d_50r<-svf(raster3m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_3m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_50r"))
#   },
   svf_3m_8d_100r = function() {svf_3m_8d_100r<-svf(raster3m, nAngles = 8, maxDist = 100, ll=F)
   writeRaster(svf_3m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_100r"))
   },
#   svf_3m_8d_200r = function(){ svf_3m_8d_200r<-svf(raster3m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_3m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_200r"))
#   },
#   svf_3m_8d_400r = function() {svf_3m_8d_400r<-svf(raster3m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_3m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_400r"))
#   },
#   svf_3m_8d_800r = function() {svf_3m_8d_800r<-svf(raster3m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_3m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_3m_16d_5r = function() {svf_3m_16d_5r<-svf(raster3m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_3m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_5r"))
#   },
#   svf_3m_16d_10r = function() {svf_3m_16d_10r<-svf(raster3m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_3m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_10r"))
#   },
#   svf_3m_16d_25r = function() { svf_3m_16d_25r <- svf(raster3m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_3m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_25r"))
#   },
#   svf_3m_16d_50r = function() {svf_3m_16d_50r<-svf(raster3m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_3m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_50r"))
#   },
#   svf_3m_16d_100r = function() {svf_3m_16d_100r<-svf(raster3m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_3m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_100r"))
#   },
#   svf_3m_16d_200r = function(){ svf_3m_16d_200r<-svf(raster3m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_3m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_200r"))
#   },
#   svf_3m_16d_400r = function() {svf_3m_16d_400r<-svf(raster3m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_3m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_400r"))
#   },
#   svf_3m_16d_800r = function() {svf_3m_16d_800r<-svf(raster3m, nAngles = 16, maxDist = 800, ll=F)
#   writeRaster(svf_3m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_16d_800r"))
#   },
#   
#   
#   
#   
#   svf_3m_32d_5r = function() {svf_3m_32d_5r<-svf(raster3m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_3m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_5r"))
#   },
#   svf_3m_32d_10r = function() {svf_3m_32d_10r<-svf(raster3m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_3m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_10r"))
#   },
#   svf_3m_32d_25r = function() { svf_3m_32d_25r <- svf(raster3m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_3m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_25r"))
#   },
#   svf_3m_32d_50r = function() {svf_3m_32d_50r<-svf(raster3m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_3m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_50r"))
#   },
#   svf_3m_32d_100r = function() {svf_3m_32d_100r<-svf(raster3m, nAngles = 32, maxDist = 100, ll=F)
#   writeRaster(svf_3m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_100r"))
#   },
#   svf_3m_32d_200r = function(){ svf_3m_32d_200r<-svf(raster3m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_3m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_200r"))
#   },
#   svf_3m_32d_400r = function() {svf_3m_32d_400r<-svf(raster3m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_3m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_400r"))
#   },
#   svf_3m_32d_800r = function() {svf_3m_32d_800r<-svf(raster3m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_3m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_3m_64d_5r = function() {svf_3m_64d_5r<-svf(raster3m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_3m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_5r"))
#   },
#   svf_3m_64d_10r = function() {svf_3m_64d_10r<-svf(raster3m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_3m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_10r"))
#   },
#   svf_3m_64d_25r = function() { svf_3m_64d_25r <- svf(raster3m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_3m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_25r"))
#   },
#   svf_3m_64d_50r = function() {svf_3m_64d_50r<-svf(raster3m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_3m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_50r"))
#   },
#   svf_3m_64d_100r = function() {svf_3m_64d_100r<-svf(raster3m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_3m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_100r"))
#   },
#   svf_3m_64d_200r = function(){ svf_3m_64d_200r<-svf(raster3m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_3m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_200r"))
#   },
#   svf_3m_64d_400r = function() {svf_3m_64d_400r<-svf(raster3m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_3m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_400r"))
#   },
#   svf_3m_64d_800r = function() {svf_3m_64d_800r<-svf(raster3m, nAngles = 64, maxDist = 800, ll=F)
#   writeRaster(svf_3m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_3m_64d_800r"))
#   },
#   
#   
#   
#   
#   
#   
#   
# # 
# # ###4m
# # 
# 
#   svf_4m_1d_5r = function() {svf_4m_1d_5r<-svf(raster4m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_4m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_5r"))
#   },
#   svf_4m_1d_10r = function() {svf_4m_1d_10r<-svf(raster4m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_4m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_10r"))
#   },
#   svf_4m_1d_25r = function() { svf_4m_1d_25r <- svf(raster4m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_4m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_25r"))
#   },
#   svf_4m_1d_50r = function() {svf_4m_1d_50r<-svf(raster4m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_4m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_50r"))
#   },
#   svf_4m_1d_100r = function() {svf_4m_1d_100r<-svf(raster4m, nAngles = 1, maxDist = 100, ll=F)
#   writeRaster(svf_4m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_100r"))
#   },
#   svf_4m_1d_200r = function(){ svf_4m_1d_200r<-svf(raster4m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_4m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_200r"))
#   },
#   svf_4m_1d_400r = function() {svf_4m_1d_400r<-svf(raster4m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_4m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_400r"))
#   },
#   svf_4m_1d_800r = function() {svf_4m_1d_800r<-svf(raster4m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_4m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_4m_2d_5r = function() {svf_4m_2d_5r<-svf(raster4m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_4m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_5r"))
#   },
#   svf_4m_2d_10r = function() {svf_4m_2d_10r<-svf(raster4m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_4m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_10r"))
#   },
#   svf_4m_2d_25r = function() { svf_4m_2d_25r <- svf(raster4m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_4m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_25r"))
#   },
#   svf_4m_2d_50r = function() {svf_4m_2d_50r<-svf(raster4m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_4m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_50r"))
#   },
#   svf_4m_2d_100r = function() {svf_4m_2d_100r<-svf(raster4m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_4m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_100r"))
#   },
#   svf_4m_2d_200r = function(){ svf_4m_2d_200r<-svf(raster4m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_4m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_200r"))
#   },
#   svf_4m_2d_400r = function() {svf_4m_2d_400r<-svf(raster4m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_4m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_400r"))
#   },
   svf_4m_2d_800r = function() {svf_4m_2d_800r<-svf(raster4m, nAngles = 2, maxDist = 800, ll=F)
   writeRaster(svf_4m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_2d_800r"))
   },
#   
#   
#   
#   
#   svf_4m_3d_5r = function() {svf_4m_3d_5r<-svf(raster4m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_4m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_5r"))
#   },
#   svf_4m_3d_10r = function() {svf_4m_3d_10r<-svf(raster4m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_4m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_10r"))
#   },
#   svf_4m_3d_25r = function() { svf_4m_3d_25r <- svf(raster4m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_4m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_25r"))
#   },
#   svf_4m_3d_50r = function() {svf_4m_3d_50r<-svf(raster4m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_4m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_50r"))
#   },
   svf_4m_3d_100r = function() {svf_4m_3d_100r<-svf(raster4m, nAngles = 3, maxDist = 100, ll=F)
   writeRaster(svf_4m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_100r"))
   },
#   svf_4m_3d_200r = function(){ svf_4m_3d_200r<-svf(raster4m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_4m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_200r"))
#   },
#   svf_4m_3d_400r = function() {svf_4m_3d_400r<-svf(raster4m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_4m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_400r"))
#   },
#   svf_4m_3d_800r = function() {svf_4m_3d_800r<-svf(raster4m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_4m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_4m_4d_5r = function() {svf_4m_4d_5r<-svf(raster4m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_4m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_5r"))
#   },
#   svf_4m_4d_10r = function() {svf_4m_4d_10r<-svf(raster4m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_4m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_10r"))
#   },
#   svf_4m_4d_25r = function() { svf_4m_4d_25r <- svf(raster4m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_4m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_25r"))
#   },
#   svf_4m_4d_50r = function() {svf_4m_4d_50r<-svf(raster4m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_4m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_50r"))
#   },
#   svf_4m_4d_100r = function() {svf_4m_4d_100r<-svf(raster4m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_4m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_100r"))
#   },
#   svf_4m_4d_200r = function(){ svf_4m_4d_200r<-svf(raster4m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_4m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_200r"))
#   },
#   svf_4m_4d_400r = function() {svf_4m_4d_400r<-svf(raster4m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_4m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_400r"))
#   },
#   svf_4m_4d_800r = function() {svf_4m_4d_800r<-svf(raster4m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_4m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_4d_800r"))
#   },
#   
#   
#   
#   svf_4m_8d_5r = function() {svf_4m_8d_5r<-svf(raster4m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_4m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_5r"))
#   },
#   svf_4m_8d_10r = function() {svf_4m_8d_10r<-svf(raster4m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_4m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_10r"))
#   },
#   svf_4m_8d_25r = function() { svf_4m_8d_25r <- svf(raster4m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_4m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_25r"))
#   },
#   svf_4m_8d_50r = function() {svf_4m_8d_50r<-svf(raster4m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_4m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_50r"))
#   },
#   svf_4m_8d_100r = function() {svf_4m_8d_100r<-svf(raster4m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_4m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_100r"))
#   },
#   svf_4m_8d_200r = function(){ svf_4m_8d_200r<-svf(raster4m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_4m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_200r"))
#   },
#   svf_4m_8d_400r = function() {svf_4m_8d_400r<-svf(raster4m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_4m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_400r"))
#   },
#   svf_4m_8d_800r = function() {svf_4m_8d_800r<-svf(raster4m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_4m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_4m_16d_5r = function() {svf_4m_16d_5r<-svf(raster4m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_4m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_5r"))
#   },
#   svf_4m_16d_10r = function() {svf_4m_16d_10r<-svf(raster4m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_4m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_10r"))
#   },
#   svf_4m_16d_25r = function() { svf_4m_16d_25r <- svf(raster4m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_4m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_25r"))
#   },
#   svf_4m_16d_50r = function() {svf_4m_16d_50r<-svf(raster4m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_4m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_50r"))
#   },
#   svf_4m_16d_100r = function() {svf_4m_16d_100r<-svf(raster4m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_4m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_100r"))
#   },
#   svf_4m_16d_200r = function(){ svf_4m_16d_200r<-svf(raster4m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_4m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_200r"))
#   },
#   svf_4m_16d_400r = function() {svf_4m_16d_400r<-svf(raster4m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_4m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_400r"))
#   },
#   svf_4m_16d_800r = function() {svf_4m_16d_800r<-svf(raster4m, nAngles = 16, maxDist = 800, ll=F)
#   writeRaster(svf_4m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_16d_800r"))
#   },
#   
#   
#   
#   
#   svf_4m_32d_5r = function() {svf_4m_32d_5r<-svf(raster4m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_4m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_5r"))
#   },
#   svf_4m_32d_10r = function() {svf_4m_32d_10r<-svf(raster4m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_4m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_10r"))
#   },
#   svf_4m_32d_25r = function() { svf_4m_32d_25r <- svf(raster4m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_4m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_25r"))
#   },
#   svf_4m_32d_50r = function() {svf_4m_32d_50r<-svf(raster4m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_4m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_50r"))
#   },
#   svf_4m_32d_100r = function() {svf_4m_32d_100r<-svf(raster4m, nAngles = 32, maxDist = 100, ll=F)
#   writeRaster(svf_4m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_100r"))
#   },
#   svf_4m_32d_200r = function(){ svf_4m_32d_200r<-svf(raster4m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_4m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_200r"))
#   },
#   svf_4m_32d_400r = function() {svf_4m_32d_400r<-svf(raster4m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_4m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_400r"))
#   },
#   svf_4m_32d_800r = function() {svf_4m_32d_800r<-svf(raster4m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_4m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_4m_64d_5r = function() {svf_4m_64d_5r<-svf(raster4m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_4m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_5r"))
#   },
#   svf_4m_64d_10r = function() {svf_4m_64d_10r<-svf(raster4m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_4m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_10r"))
#   },
#   svf_4m_64d_25r = function() { svf_4m_64d_25r <- svf(raster4m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_4m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_25r"))
#   },
#   svf_4m_64d_50r = function() {svf_4m_64d_50r<-svf(raster4m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_4m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_50r"))
#   },
#   svf_4m_64d_100r = function() {svf_4m_64d_100r<-svf(raster4m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_4m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_100r"))
#   },
#   svf_4m_64d_200r = function(){ svf_4m_64d_200r<-svf(raster4m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_4m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_200r"))
#   },
#   svf_4m_64d_400r = function() {svf_4m_64d_400r<-svf(raster4m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_4m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_400r"))
#   },
   svf_4m_64d_800r = function() {svf_4m_64d_800r<-svf(raster4m, nAngles = 64, maxDist = 800, ll=F)
   writeRaster(svf_4m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_4m_64d_800r"))
   },
#   
#   
#   
#   
#   
#   
#   
#   
# # 
# # ###5m
# # 
# 
#   svf_5m_1d_5r = function() {svf_5m_1d_5r<-svf(raster5m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_5m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_5r"))
#   },
#   svf_5m_1d_10r = function() {svf_5m_1d_10r<-svf(raster5m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_5m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_10r"))
#   },
#   svf_5m_1d_25r = function() { svf_5m_1d_25r <- svf(raster5m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_5m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_25r"))
#   },
#   svf_5m_1d_50r = function() {svf_5m_1d_50r<-svf(raster5m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_5m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_50r"))
#   },
   svf_5m_1d_100r = function() {svf_5m_1d_100r<-svf(raster5m, nAngles = 1, maxDist = 100, ll=F)
   writeRaster(svf_5m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_100r"))
   },
#   svf_5m_1d_200r = function(){ svf_5m_1d_200r<-svf(raster5m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_5m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_200r"))
#   },
#   svf_5m_1d_400r = function() {svf_5m_1d_400r<-svf(raster5m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_5m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_400r"))
#   },
#   svf_5m_1d_800r = function() {svf_5m_1d_800r<-svf(raster5m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_5m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_5m_2d_5r = function() {svf_5m_2d_5r<-svf(raster5m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_5m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_5r"))
#   },
#   svf_5m_2d_10r = function() {svf_5m_2d_10r<-svf(raster5m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_5m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_10r"))
#   },
#   svf_5m_2d_25r = function() { svf_5m_2d_25r <- svf(raster5m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_5m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_25r"))
#   },
#   svf_5m_2d_50r = function() {svf_5m_2d_50r<-svf(raster5m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_5m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_50r"))
#   },
#   svf_5m_2d_100r = function() {svf_5m_2d_100r<-svf(raster5m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_5m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_100r"))
#   },
#   svf_5m_2d_200r = function(){ svf_5m_2d_200r<-svf(raster5m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_5m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_200r"))
#   },
#   svf_5m_2d_400r = function() {svf_5m_2d_400r<-svf(raster5m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_5m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_400r"))
#   },
#   svf_5m_2d_800r = function() {svf_5m_2d_800r<-svf(raster5m, nAngles = 2, maxDist = 800, ll=F)
#   writeRaster(svf_5m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_2d_800r"))
#   },
#   
#   
#   
#   
#   svf_5m_3d_5r = function() {svf_5m_3d_5r<-svf(raster5m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_5m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_5r"))
#   },
#   svf_5m_3d_10r = function() {svf_5m_3d_10r<-svf(raster5m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_5m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_10r"))
#   },
#   svf_5m_3d_25r = function() { svf_5m_3d_25r <- svf(raster5m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_5m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_25r"))
#   },
#   svf_5m_3d_50r = function() {svf_5m_3d_50r<-svf(raster5m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_5m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_50r"))
#   },
#   svf_5m_3d_100r = function() {svf_5m_3d_100r<-svf(raster5m, nAngles = 3, maxDist = 100, ll=F)
#   writeRaster(svf_5m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_100r"))
#   },
#   svf_5m_3d_200r = function(){ svf_5m_3d_200r<-svf(raster5m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_5m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_200r"))
#   },
#   svf_5m_3d_400r = function() {svf_5m_3d_400r<-svf(raster5m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_5m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_400r"))
#   },
#   svf_5m_3d_800r = function() {svf_5m_3d_800r<-svf(raster5m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_5m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_5m_4d_5r = function() {svf_5m_4d_5r<-svf(raster5m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_5m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_5r"))
#   },
#   svf_5m_4d_10r = function() {svf_5m_4d_10r<-svf(raster5m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_5m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_10r"))
#   },
#   svf_5m_4d_25r = function() { svf_5m_4d_25r <- svf(raster5m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_5m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_25r"))
#   },
#   svf_5m_4d_50r = function() {svf_5m_4d_50r<-svf(raster5m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_5m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_50r"))
#   },
#   svf_5m_4d_100r = function() {svf_5m_4d_100r<-svf(raster5m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_5m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_100r"))
#   },
#   svf_5m_4d_200r = function(){ svf_5m_4d_200r<-svf(raster5m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_5m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_200r"))
#   },
#   svf_5m_4d_400r = function() {svf_5m_4d_400r<-svf(raster5m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_5m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_400r"))
#   },
#   svf_5m_4d_800r = function() {svf_5m_4d_800r<-svf(raster5m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_5m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_4d_800r"))
#   },
#   
#   
#   
#   svf_5m_8d_5r = function() {svf_5m_8d_5r<-svf(raster5m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_5m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_5r"))
#   },
#   svf_5m_8d_10r = function() {svf_5m_8d_10r<-svf(raster5m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_5m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_10r"))
#   },
#   svf_5m_8d_25r = function() { svf_5m_8d_25r <- svf(raster5m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_5m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_25r"))
#   },
#   svf_5m_8d_50r = function() {svf_5m_8d_50r<-svf(raster5m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_5m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_50r"))
#   },
#   svf_5m_8d_100r = function() {svf_5m_8d_100r<-svf(raster5m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_5m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_100r"))
#   },
#   svf_5m_8d_200r = function(){ svf_5m_8d_200r<-svf(raster5m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_5m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_200r"))
#   },
#   svf_5m_8d_400r = function() {svf_5m_8d_400r<-svf(raster5m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_5m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_400r"))
#   },
#   svf_5m_8d_800r = function() {svf_5m_8d_800r<-svf(raster5m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_5m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_5m_16d_5r = function() {svf_5m_16d_5r<-svf(raster5m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_5m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_5r"))
#   },
#   svf_5m_16d_10r = function() {svf_5m_16d_10r<-svf(raster5m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_5m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_10r"))
#   },
#   svf_5m_16d_25r = function() { svf_5m_16d_25r <- svf(raster5m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_5m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_25r"))
#   },
#   svf_5m_16d_50r = function() {svf_5m_16d_50r<-svf(raster5m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_5m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_50r"))
#   },
#   svf_5m_16d_100r = function() {svf_5m_16d_100r<-svf(raster5m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_5m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_100r"))
#   },
#   svf_5m_16d_200r = function(){ svf_5m_16d_200r<-svf(raster5m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_5m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_200r"))
#   },
#   svf_5m_16d_400r = function() {svf_5m_16d_400r<-svf(raster5m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_5m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_400r"))
#   },
   svf_5m_16d_800r = function() {svf_5m_16d_800r<-svf(raster5m, nAngles = 16, maxDist = 800, ll=F)
   writeRaster(svf_5m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_16d_800r"))
   },
#   
#   
#   
#   
#   svf_5m_32d_5r = function() {svf_5m_32d_5r<-svf(raster5m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_5m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_5r"))
#   },
#   svf_5m_32d_10r = function() {svf_5m_32d_10r<-svf(raster5m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_5m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_10r"))
#   },
#   svf_5m_32d_25r = function() { svf_5m_32d_25r <- svf(raster5m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_5m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_25r"))
#   },
#   svf_5m_32d_50r = function() {svf_5m_32d_50r<-svf(raster5m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_5m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_50r"))
#   },
   svf_5m_32d_100r = function() {svf_5m_32d_100r<-svf(raster5m, nAngles = 32, maxDist = 100, ll=F)
   writeRaster(svf_5m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_100r"))
   },
#   svf_5m_32d_200r = function(){ svf_5m_32d_200r<-svf(raster5m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_5m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_200r"))
#   },
#   svf_5m_32d_400r = function() {svf_5m_32d_400r<-svf(raster5m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_5m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_400r"))
#   },
#   svf_5m_32d_800r = function() {svf_5m_32d_800r<-svf(raster5m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_5m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_5m_64d_5r = function() {svf_5m_64d_5r<-svf(raster5m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_5m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_5r"))
#   },
#   svf_5m_64d_10r = function() {svf_5m_64d_10r<-svf(raster5m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_5m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_10r"))
#   },
#   svf_5m_64d_25r = function() { svf_5m_64d_25r <- svf(raster5m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_5m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_25r"))
#   },
#   svf_5m_64d_50r = function() {svf_5m_64d_50r<-svf(raster5m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_5m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_50r"))
#   },
#   svf_5m_64d_100r = function() {svf_5m_64d_100r<-svf(raster5m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_5m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_100r"))
#   },
#   svf_5m_64d_200r = function(){ svf_5m_64d_200r<-svf(raster5m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_5m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_200r"))
#   },
#   svf_5m_64d_400r = function() {svf_5m_64d_400r<-svf(raster5m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_5m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_400r"))
#   },
#   svf_5m_64d_800r = function() {svf_5m_64d_800r<-svf(raster5m, nAngles = 64, maxDist = 800, ll=F)
#   writeRaster(svf_5m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_5m_64d_800r"))
#   },
#   
#   
#   
#   
#   
#   
#   
# # 
# # 
# # ###10m
# # 
#   svf_10m_1d_5r = function() {svf_10m_1d_5r<-svf(raster10m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_10m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_5r"))
#   },
#   svf_10m_1d_10r = function() {svf_10m_1d_10r<-svf(raster10m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_10m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_10r"))
#   },
#   svf_10m_1d_25r = function() { svf_10m_1d_25r <- svf(raster10m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_10m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_25r"))
#   },
#   svf_10m_1d_50r = function() {svf_10m_1d_50r<-svf(raster10m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_10m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_50r"))
#   },
#   svf_10m_1d_100r = function() {svf_10m_1d_100r<-svf(raster10m, nAngles = 1, maxDist = 100, ll=F)
#   writeRaster(svf_10m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_100r"))
#   },
#   svf_10m_1d_200r = function(){ svf_10m_1d_200r<-svf(raster10m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_10m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_200r"))
#   },
#   svf_10m_1d_400r = function() {svf_10m_1d_400r<-svf(raster10m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_10m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_400r"))
#   },
#   svf_10m_1d_800r = function() {svf_10m_1d_800r<-svf(raster10m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_10m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_10m_2d_5r = function() {svf_10m_2d_5r<-svf(raster10m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_10m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_5r"))
#   },
#   svf_10m_2d_10r = function() {svf_10m_2d_10r<-svf(raster10m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_10m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_10r"))
#   },
#   svf_10m_2d_25r = function() { svf_10m_2d_25r <- svf(raster10m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_10m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_25r"))
#   },
#   svf_10m_2d_50r = function() {svf_10m_2d_50r<-svf(raster10m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_10m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_50r"))
#   },
#   svf_10m_2d_100r = function() {svf_10m_2d_100r<-svf(raster10m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_10m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_100r"))
#   },
#   svf_10m_2d_200r = function(){ svf_10m_2d_200r<-svf(raster10m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_10m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_200r"))
#   },
#   svf_10m_2d_400r = function() {svf_10m_2d_400r<-svf(raster10m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_10m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_400r"))
#   },
#   svf_10m_2d_800r = function() {svf_10m_2d_800r<-svf(raster10m, nAngles = 2, maxDist = 800, ll=F)
#   writeRaster(svf_10m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_2d_800r"))
#   },
#   
#   
#   
#   
#   svf_10m_3d_5r = function() {svf_10m_3d_5r<-svf(raster10m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_10m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_5r"))
#   },
#   svf_10m_3d_10r = function() {svf_10m_3d_10r<-svf(raster10m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_10m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_10r"))
#   },
#   svf_10m_3d_25r = function() { svf_10m_3d_25r <- svf(raster10m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_10m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_25r"))
#   },
#   svf_10m_3d_50r = function() {svf_10m_3d_50r<-svf(raster10m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_10m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_50r"))
#   },
#   svf_10m_3d_100r = function() {svf_10m_3d_100r<-svf(raster10m, nAngles = 3, maxDist = 100, ll=F)
#   writeRaster(svf_10m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_100r"))
#   },
#   svf_10m_3d_200r = function(){ svf_10m_3d_200r<-svf(raster10m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_10m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_200r"))
#   },
#   svf_10m_3d_400r = function() {svf_10m_3d_400r<-svf(raster10m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_10m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_400r"))
#   },
#   svf_10m_3d_800r = function() {svf_10m_3d_800r<-svf(raster10m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_10m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_10m_4d_5r = function() {svf_10m_4d_5r<-svf(raster10m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_10m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_5r"))
#   },
#   svf_10m_4d_10r = function() {svf_10m_4d_10r<-svf(raster10m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_10m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_10r"))
#   },
#   svf_10m_4d_25r = function() { svf_10m_4d_25r <- svf(raster10m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_10m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_25r"))
#   },
#   svf_10m_4d_50r = function() {svf_10m_4d_50r<-svf(raster10m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_10m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_50r"))
#   },
#   svf_10m_4d_100r = function() {svf_10m_4d_100r<-svf(raster10m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_10m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_100r"))
#   },
#   svf_10m_4d_200r = function(){ svf_10m_4d_200r<-svf(raster10m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_10m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_200r"))
#   },
#   svf_10m_4d_400r = function() {svf_10m_4d_400r<-svf(raster10m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_10m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_400r"))
#   },
   svf_10m_4d_800r = function() {svf_10m_4d_800r<-svf(raster10m, nAngles = 4, maxDist = 800, ll=F)
   writeRaster(svf_10m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_4d_800r"))
   },
#   
#   
#   
#   svf_10m_8d_5r = function() {svf_10m_8d_5r<-svf(raster10m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_10m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_5r"))
#   },
#   svf_10m_8d_10r = function() {svf_10m_8d_10r<-svf(raster10m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_10m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_10r"))
#   },
#   svf_10m_8d_25r = function() { svf_10m_8d_25r <- svf(raster10m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_10m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_25r"))
#   },
#   svf_10m_8d_50r = function() {svf_10m_8d_50r<-svf(raster10m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_10m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_50r"))
#   },
   svf_10m_8d_100r = function() {svf_10m_8d_100r<-svf(raster10m, nAngles = 8, maxDist = 100, ll=F)
   writeRaster(svf_10m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_100r"))
   },
#   svf_10m_8d_200r = function(){ svf_10m_8d_200r<-svf(raster10m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_10m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_200r"))
#   },
#   svf_10m_8d_400r = function() {svf_10m_8d_400r<-svf(raster10m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_10m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_400r"))
#   },
#   svf_10m_8d_800r = function() {svf_10m_8d_800r<-svf(raster10m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_10m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_10m_16d_5r = function() {svf_10m_16d_5r<-svf(raster10m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_10m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_5r"))
#   },
#   svf_10m_16d_10r = function() {svf_10m_16d_10r<-svf(raster10m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_10m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_10r"))
#   },
#   svf_10m_16d_25r = function() { svf_10m_16d_25r <- svf(raster10m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_10m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_25r"))
#   },
#   svf_10m_16d_50r = function() {svf_10m_16d_50r<-svf(raster10m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_10m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_50r"))
#   },
#   svf_10m_16d_100r = function() {svf_10m_16d_100r<-svf(raster10m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_10m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_100r"))
#   },
#   svf_10m_16d_200r = function(){ svf_10m_16d_200r<-svf(raster10m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_10m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_200r"))
#   },
#   svf_10m_16d_400r = function() {svf_10m_16d_400r<-svf(raster10m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_10m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_400r"))
#   },
#   svf_10m_16d_800r = function() {svf_10m_16d_800r<-svf(raster10m, nAngles = 16, maxDist = 800, ll=F)
#   writeRaster(svf_10m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_16d_800r"))
#   },
#   
#   
#   
#   
#   svf_10m_32d_5r = function() {svf_10m_32d_5r<-svf(raster10m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_10m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_5r"))
#   },
#   svf_10m_32d_10r = function() {svf_10m_32d_10r<-svf(raster10m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_10m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_10r"))
#   },
#   svf_10m_32d_25r = function() { svf_10m_32d_25r <- svf(raster10m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_10m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_25r"))
#   },
#   svf_10m_32d_50r = function() {svf_10m_32d_50r<-svf(raster10m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_10m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_50r"))
#   },
#   svf_10m_32d_100r = function() {svf_10m_32d_100r<-svf(raster10m, nAngles = 32, maxDist = 100, ll=F)
#   writeRaster(svf_10m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_100r"))
#   },
#   svf_10m_32d_200r = function(){ svf_10m_32d_200r<-svf(raster10m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_10m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_200r"))
#   },
#   svf_10m_32d_400r = function() {svf_10m_32d_400r<-svf(raster10m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_10m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_400r"))
#   },
#   svf_10m_32d_800r = function() {svf_10m_32d_800r<-svf(raster10m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_10m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_10m_64d_5r = function() {svf_10m_64d_5r<-svf(raster10m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_10m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_5r"))
#   },
#   svf_10m_64d_10r = function() {svf_10m_64d_10r<-svf(raster10m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_10m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_10r"))
#   },
#   svf_10m_64d_25r = function() { svf_10m_64d_25r <- svf(raster10m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_10m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_25r"))
#   },
#   svf_10m_64d_50r = function() {svf_10m_64d_50r<-svf(raster10m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_10m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_50r"))
#   },
#   svf_10m_64d_100r = function() {svf_10m_64d_100r<-svf(raster10m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_10m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_100r"))
#   },
#   svf_10m_64d_200r = function(){ svf_10m_64d_200r<-svf(raster10m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_10m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_200r"))
#   },
#   svf_10m_64d_400r = function() {svf_10m_64d_400r<-svf(raster10m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_10m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_400r"))
#   },
#   svf_10m_64d_800r = function() {svf_10m_64d_800r<-svf(raster10m, nAngles = 64, maxDist = 800, ll=F)
#   writeRaster(svf_10m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_10m_64d_800r"))
#   },
#   
#   
#   
#   
#   
#   
#   
# # 
# # 
# # ###20m
# # 
# 
#   svf_20m_1d_5r = function() {svf_20m_1d_5r<-svf(raster20m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_20m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_5r"))
#   },
#   svf_20m_1d_10r = function() {svf_20m_1d_10r<-svf(raster20m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_20m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_10r"))
#   },
#   svf_20m_1d_25r = function() { svf_20m_1d_25r <- svf(raster20m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_20m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_25r"))
#   },
#   svf_20m_1d_50r = function() {svf_20m_1d_50r<-svf(raster20m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_20m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_50r"))
#   },
#   svf_20m_1d_100r = function() {svf_20m_1d_100r<-svf(raster20m, nAngles = 1, maxDist = 100, ll=F)
#   writeRaster(svf_20m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_100r"))
#   },
#   svf_20m_1d_200r = function(){ svf_20m_1d_200r<-svf(raster20m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_20m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_200r"))
#   },
#   svf_20m_1d_400r = function() {svf_20m_1d_400r<-svf(raster20m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_20m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_400r"))
#   },
#   svf_20m_1d_800r = function() {svf_20m_1d_800r<-svf(raster20m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_20m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_20m_2d_5r = function() {svf_20m_2d_5r<-svf(raster20m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_20m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_5r"))
#   },
#   svf_20m_2d_10r = function() {svf_20m_2d_10r<-svf(raster20m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_20m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_10r"))
#   },
#   svf_20m_2d_25r = function() { svf_20m_2d_25r <- svf(raster20m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_20m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_25r"))
#   },
#   svf_20m_2d_50r = function() {svf_20m_2d_50r<-svf(raster20m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_20m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_50r"))
#   },
#   svf_20m_2d_100r = function() {svf_20m_2d_100r<-svf(raster20m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_20m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_100r"))
#   },
#   svf_20m_2d_200r = function(){ svf_20m_2d_200r<-svf(raster20m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_20m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_200r"))
#   },
#   svf_20m_2d_400r = function() {svf_20m_2d_400r<-svf(raster20m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_20m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_400r"))
#   },
   svf_20m_2d_800r = function() {svf_20m_2d_800r<-svf(raster20m, nAngles = 2, maxDist = 800, ll=F)
   writeRaster(svf_20m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_2d_800r"))
   },
#   
#   
#   
#   
#   svf_20m_3d_5r = function() {svf_20m_3d_5r<-svf(raster20m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_20m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_5r"))
#   },
#   svf_20m_3d_10r = function() {svf_20m_3d_10r<-svf(raster20m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_20m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_10r"))
#   },
#   svf_20m_3d_25r = function() { svf_20m_3d_25r <- svf(raster20m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_20m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_25r"))
#   },
#   svf_20m_3d_50r = function() {svf_20m_3d_50r<-svf(raster20m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_20m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_50r"))
#   },
   svf_20m_3d_100r = function() {svf_20m_3d_100r<-svf(raster20m, nAngles = 3, maxDist = 100, ll=F)
   writeRaster(svf_20m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_100r"))
   },
#   svf_20m_3d_200r = function(){ svf_20m_3d_200r<-svf(raster20m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_20m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_200r"))
#   },
#   svf_20m_3d_400r = function() {svf_20m_3d_400r<-svf(raster20m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_20m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_400r"))
#   },
#   svf_20m_3d_800r = function() {svf_20m_3d_800r<-svf(raster20m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_20m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_20m_4d_5r = function() {svf_20m_4d_5r<-svf(raster20m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_20m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_5r"))
#   },
#   svf_20m_4d_10r = function() {svf_20m_4d_10r<-svf(raster20m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_20m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_10r"))
#   },
#   svf_20m_4d_25r = function() { svf_20m_4d_25r <- svf(raster20m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_20m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_25r"))
#   },
#   svf_20m_4d_50r = function() {svf_20m_4d_50r<-svf(raster20m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_20m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_50r"))
#   },
#   svf_20m_4d_100r = function() {svf_20m_4d_100r<-svf(raster20m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_20m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_100r"))
#   },
#   svf_20m_4d_200r = function(){ svf_20m_4d_200r<-svf(raster20m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_20m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_200r"))
#   },
#   svf_20m_4d_400r = function() {svf_20m_4d_400r<-svf(raster20m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_20m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_400r"))
#   },
#   svf_20m_4d_800r = function() {svf_20m_4d_800r<-svf(raster20m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_20m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_4d_800r"))
#   },
#   
#   
#   
#   svf_20m_8d_5r = function() {svf_20m_8d_5r<-svf(raster20m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_20m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_5r"))
#   },
#   svf_20m_8d_10r = function() {svf_20m_8d_10r<-svf(raster20m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_20m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_10r"))
#   },
#   svf_20m_8d_25r = function() { svf_20m_8d_25r <- svf(raster20m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_20m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_25r"))
#   },
#   svf_20m_8d_50r = function() {svf_20m_8d_50r<-svf(raster20m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_20m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_50r"))
#   },
#   svf_20m_8d_100r = function() {svf_20m_8d_100r<-svf(raster20m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_20m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_100r"))
#   },
#   svf_20m_8d_200r = function(){ svf_20m_8d_200r<-svf(raster20m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_20m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_200r"))
#   },
#   svf_20m_8d_400r = function() {svf_20m_8d_400r<-svf(raster20m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_20m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_400r"))
#   },
#   svf_20m_8d_800r = function() {svf_20m_8d_800r<-svf(raster20m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_20m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_20m_16d_5r = function() {svf_20m_16d_5r<-svf(raster20m, nAngles = 16, maxDist = 5, ll=F)
#   writeRaster(svf_20m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_5r"))
#   },
#   svf_20m_16d_10r = function() {svf_20m_16d_10r<-svf(raster20m, nAngles = 16, maxDist = 10, ll=F)
#   writeRaster(svf_20m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_10r"))
#   },
#   svf_20m_16d_25r = function() { svf_20m_16d_25r <- svf(raster20m, nAngles = 16, maxDist = 25, ll=F)
#   writeRaster(svf_20m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_25r"))
#   },
#   svf_20m_16d_50r = function() {svf_20m_16d_50r<-svf(raster20m, nAngles = 16, maxDist = 50, ll=F)
#   writeRaster(svf_20m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_50r"))
#   },
#   svf_20m_16d_100r = function() {svf_20m_16d_100r<-svf(raster20m, nAngles = 16, maxDist = 100, ll=F)
#   writeRaster(svf_20m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_100r"))
#   },
#   svf_20m_16d_200r = function(){ svf_20m_16d_200r<-svf(raster20m, nAngles = 16, maxDist = 200, ll=F)
#   writeRaster(svf_20m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_200r"))
#   },
#   svf_20m_16d_400r = function() {svf_20m_16d_400r<-svf(raster20m, nAngles = 16, maxDist = 400, ll=F)
#   writeRaster(svf_20m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_400r"))
#   },
#   svf_20m_16d_800r = function() {svf_20m_16d_800r<-svf(raster20m, nAngles = 16, maxDist = 800, ll=F)
#   writeRaster(svf_20m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_16d_800r"))
#   },
#   
#   
#   
#   
#   svf_20m_32d_5r = function() {svf_20m_32d_5r<-svf(raster20m, nAngles = 32, maxDist = 5, ll=F)
#   writeRaster(svf_20m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_5r"))
#   },
#   svf_20m_32d_10r = function() {svf_20m_32d_10r<-svf(raster20m, nAngles = 32, maxDist = 10, ll=F)
#   writeRaster(svf_20m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_10r"))
#   },
#   svf_20m_32d_25r = function() { svf_20m_32d_25r <- svf(raster20m, nAngles = 32, maxDist = 25, ll=F)
#   writeRaster(svf_20m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_25r"))
#   },
#   svf_20m_32d_50r = function() {svf_20m_32d_50r<-svf(raster20m, nAngles = 32, maxDist = 50, ll=F)
#   writeRaster(svf_20m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_50r"))
#   },
#   svf_20m_32d_100r = function() {svf_20m_32d_100r<-svf(raster20m, nAngles = 32, maxDist = 100, ll=F)
#   writeRaster(svf_20m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_100r"))
#   },
#   svf_20m_32d_200r = function(){ svf_20m_32d_200r<-svf(raster20m, nAngles = 32, maxDist = 200, ll=F)
#   writeRaster(svf_20m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_200r"))
#   },
#   svf_20m_32d_400r = function() {svf_20m_32d_400r<-svf(raster20m, nAngles = 32, maxDist = 400, ll=F)
#   writeRaster(svf_20m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_400r"))
#   },
#   svf_20m_32d_800r = function() {svf_20m_32d_800r<-svf(raster20m, nAngles = 32, maxDist = 800, ll=F)
#   writeRaster(svf_20m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_32d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_20m_64d_5r = function() {svf_20m_64d_5r<-svf(raster20m, nAngles = 64, maxDist = 5, ll=F)
#   writeRaster(svf_20m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_5r"))
#   },
#   svf_20m_64d_10r = function() {svf_20m_64d_10r<-svf(raster20m, nAngles = 64, maxDist = 10, ll=F)
#   writeRaster(svf_20m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_10r"))
#   },
#   svf_20m_64d_25r = function() { svf_20m_64d_25r <- svf(raster20m, nAngles = 64, maxDist = 25, ll=F)
#   writeRaster(svf_20m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_25r"))
#   },
#   svf_20m_64d_50r = function() {svf_20m_64d_50r<-svf(raster20m, nAngles = 64, maxDist = 50, ll=F)
#   writeRaster(svf_20m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_50r"))
#   },
#   svf_20m_64d_100r = function() {svf_20m_64d_100r<-svf(raster20m, nAngles = 64, maxDist = 100, ll=F)
#   writeRaster(svf_20m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_100r"))
#   },
#   svf_20m_64d_200r = function(){ svf_20m_64d_200r<-svf(raster20m, nAngles = 64, maxDist = 200, ll=F)
#   writeRaster(svf_20m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_200r"))
#   },
#   svf_20m_64d_400r = function() {svf_20m_64d_400r<-svf(raster20m, nAngles = 64, maxDist = 400, ll=F)
#   writeRaster(svf_20m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_400r"))
#   },
   svf_20m_64d_800r = function() {svf_20m_64d_800r<-svf(raster20m, nAngles = 64, maxDist = 800, ll=F)
   writeRaster(svf_20m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_20m_64d_800r"))
   },
#   
#   
#   
#   
#   
#   
#   
# # 
# # 
# # ####50m
# # 
# # 
# 
# 
#   svf_50m_1d_5r = function() {svf_50m_1d_5r<-svf(raster50m, nAngles = 1, maxDist = 5, ll=F)
#   writeRaster(svf_50m_1d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_5r"))
#   },
#   svf_50m_1d_10r = function() {svf_50m_1d_10r<-svf(raster50m, nAngles = 1, maxDist = 10, ll=F)
#   writeRaster(svf_50m_1d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_10r"))
#   },
#   svf_50m_1d_25r = function() { svf_50m_1d_25r <- svf(raster50m, nAngles = 1, maxDist = 25, ll=F)
#   writeRaster(svf_50m_1d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_25r"))
#   },
#   svf_50m_1d_50r = function() {svf_50m_1d_50r<-svf(raster50m, nAngles = 1, maxDist = 50, ll=F)
#   writeRaster(svf_50m_1d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_50r"))
#   },
   svf_50m_1d_100r = function() {svf_50m_1d_100r<-svf(raster50m, nAngles = 1, maxDist = 100, ll=F)
   writeRaster(svf_50m_1d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_100r"))
   },
#   svf_50m_1d_200r = function(){ svf_50m_1d_200r<-svf(raster50m, nAngles = 1, maxDist = 200, ll=F)
#   writeRaster(svf_50m_1d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_200r"))
#   },
#   svf_50m_1d_400r = function() {svf_50m_1d_400r<-svf(raster50m, nAngles = 1, maxDist = 400, ll=F)
#   writeRaster(svf_50m_1d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_400r"))
#   },
#   svf_50m_1d_800r = function() {svf_50m_1d_800r<-svf(raster50m, nAngles = 1, maxDist = 800, ll=F)
#   writeRaster(svf_50m_1d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_1d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_50m_2d_5r = function() {svf_50m_2d_5r<-svf(raster50m, nAngles = 2, maxDist = 5, ll=F)
#   writeRaster(svf_50m_2d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_5r"))
#   },
#   svf_50m_2d_10r = function() {svf_50m_2d_10r<-svf(raster50m, nAngles = 2, maxDist = 10, ll=F)
#   writeRaster(svf_50m_2d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_10r"))
#   },
#   svf_50m_2d_25r = function() { svf_50m_2d_25r <- svf(raster50m, nAngles = 2, maxDist = 25, ll=F)
#   writeRaster(svf_50m_2d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_25r"))
#   },
#   svf_50m_2d_50r = function() {svf_50m_2d_50r<-svf(raster50m, nAngles = 2, maxDist = 50, ll=F)
#   writeRaster(svf_50m_2d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_50r"))
#   },
#   svf_50m_2d_100r = function() {svf_50m_2d_100r<-svf(raster50m, nAngles = 2, maxDist = 100, ll=F)
#   writeRaster(svf_50m_2d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_100r"))
#   },
#   svf_50m_2d_200r = function(){ svf_50m_2d_200r<-svf(raster50m, nAngles = 2, maxDist = 200, ll=F)
#   writeRaster(svf_50m_2d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_200r"))
#   },
#   svf_50m_2d_400r = function() {svf_50m_2d_400r<-svf(raster50m, nAngles = 2, maxDist = 400, ll=F)
#   writeRaster(svf_50m_2d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_400r"))
#   },
#   svf_50m_2d_800r = function() {svf_50m_2d_800r<-svf(raster50m, nAngles = 2, maxDist = 800, ll=F)
#   writeRaster(svf_50m_2d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_2d_800r"))
#   },
#   
#   
#   
#   
#   svf_50m_3d_5r = function() {svf_50m_3d_5r<-svf(raster50m, nAngles = 3, maxDist = 5, ll=F)
#   writeRaster(svf_50m_3d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_5r"))
#   },
#   svf_50m_3d_10r = function() {svf_50m_3d_10r<-svf(raster50m, nAngles = 3, maxDist = 10, ll=F)
#   writeRaster(svf_50m_3d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_10r"))
#   },
#   svf_50m_3d_25r = function() { svf_50m_3d_25r <- svf(raster50m, nAngles = 3, maxDist = 25, ll=F)
#   writeRaster(svf_50m_3d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_25r"))
#   },
#   svf_50m_3d_50r = function() {svf_50m_3d_50r<-svf(raster50m, nAngles = 3, maxDist = 50, ll=F)
#   writeRaster(svf_50m_3d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_50r"))
#   },
#   svf_50m_3d_100r = function() {svf_50m_3d_100r<-svf(raster50m, nAngles = 3, maxDist = 100, ll=F)
#   writeRaster(svf_50m_3d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_100r"))
#   },
#   svf_50m_3d_200r = function(){ svf_50m_3d_200r<-svf(raster50m, nAngles = 3, maxDist = 200, ll=F)
#   writeRaster(svf_50m_3d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_200r"))
#   },
#   svf_50m_3d_400r = function() {svf_50m_3d_400r<-svf(raster50m, nAngles = 3, maxDist = 400, ll=F)
#   writeRaster(svf_50m_3d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_400r"))
#   },
#   svf_50m_3d_800r = function() {svf_50m_3d_800r<-svf(raster50m, nAngles = 3, maxDist = 800, ll=F)
#   writeRaster(svf_50m_3d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_3d_800r"))
#   },
#   
#   
#   
#   
#   svf_50m_4d_5r = function() {svf_50m_4d_5r<-svf(raster50m, nAngles = 4, maxDist = 5, ll=F)
#   writeRaster(svf_50m_4d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_5r"))
#   },
#   svf_50m_4d_10r = function() {svf_50m_4d_10r<-svf(raster50m, nAngles = 4, maxDist = 10, ll=F)
#   writeRaster(svf_50m_4d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_10r"))
#   },
#   svf_50m_4d_25r = function() { svf_50m_4d_25r <- svf(raster50m, nAngles = 4, maxDist = 25, ll=F)
#   writeRaster(svf_50m_4d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_25r"))
#   },
#   svf_50m_4d_50r = function() {svf_50m_4d_50r<-svf(raster50m, nAngles = 4, maxDist = 50, ll=F)
#   writeRaster(svf_50m_4d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_50r"))
#   },
#   svf_50m_4d_100r = function() {svf_50m_4d_100r<-svf(raster50m, nAngles = 4, maxDist = 100, ll=F)
#   writeRaster(svf_50m_4d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_100r"))
#   },
#   svf_50m_4d_200r = function(){ svf_50m_4d_200r<-svf(raster50m, nAngles = 4, maxDist = 200, ll=F)
#   writeRaster(svf_50m_4d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_200r"))
#   },
#   svf_50m_4d_400r = function() {svf_50m_4d_400r<-svf(raster50m, nAngles = 4, maxDist = 400, ll=F)
#   writeRaster(svf_50m_4d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_400r"))
#   },
#   svf_50m_4d_800r = function() {svf_50m_4d_800r<-svf(raster50m, nAngles = 4, maxDist = 800, ll=F)
#   writeRaster(svf_50m_4d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_4d_800r"))
#   },
# 
#   
#   
#   svf_50m_8d_5r = function() {svf_50m_8d_5r<-svf(raster50m, nAngles = 8, maxDist = 5, ll=F)
#   writeRaster(svf_50m_8d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_5r"))
#   },
#   svf_50m_8d_10r = function() {svf_50m_8d_10r<-svf(raster50m, nAngles = 8, maxDist = 10, ll=F)
#   writeRaster(svf_50m_8d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_10r"))
#   },
#   svf_50m_8d_25r = function() { svf_50m_8d_25r <- svf(raster50m, nAngles = 8, maxDist = 25, ll=F)
#   writeRaster(svf_50m_8d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_25r"))
#   },
#   svf_50m_8d_50r = function() {svf_50m_8d_50r<-svf(raster50m, nAngles = 8, maxDist = 50, ll=F)
#   writeRaster(svf_50m_8d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_50r"))
#   },
#   svf_50m_8d_100r = function() {svf_50m_8d_100r<-svf(raster50m, nAngles = 8, maxDist = 100, ll=F)
#   writeRaster(svf_50m_8d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_100r"))
#   },
#   svf_50m_8d_200r = function(){ svf_50m_8d_200r<-svf(raster50m, nAngles = 8, maxDist = 200, ll=F)
#   writeRaster(svf_50m_8d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_200r"))
#   },
#   svf_50m_8d_400r = function() {svf_50m_8d_400r<-svf(raster50m, nAngles = 8, maxDist = 400, ll=F)
#   writeRaster(svf_50m_8d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_400r"))
#   },
#   svf_50m_8d_800r = function() {svf_50m_8d_800r<-svf(raster50m, nAngles = 8, maxDist = 800, ll=F)
#   writeRaster(svf_50m_8d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_8d_800r"))
#   },
#   
#   
#   
#   
#   
#   svf_50m_16d_5r = function() {svf_50m_16d_5r<-svf(raster50m, nAngles = 16, maxDist = 5, ll=F)
#     writeRaster(svf_50m_16d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_5r"))
#   },
#   svf_50m_16d_10r = function() {svf_50m_16d_10r<-svf(raster50m, nAngles = 16, maxDist = 10, ll=F)
#     writeRaster(svf_50m_16d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_10r"))
#   },
#  svf_50m_16d_25r = function() { svf_50m_16d_25r <- svf(raster50m, nAngles = 16, maxDist = 25, ll=F)
#    writeRaster(svf_50m_16d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_25r"))
#  },
#  svf_50m_16d_50r = function() {svf_50m_16d_50r<-svf(raster50m, nAngles = 16, maxDist = 50, ll=F)
#    writeRaster(svf_50m_16d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_50r"))
#  },
#  svf_50m_16d_100r = function() {svf_50m_16d_100r<-svf(raster50m, nAngles = 16, maxDist = 100, ll=F)
#    writeRaster(svf_50m_16d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_100r"))
#  },
#  svf_50m_16d_200r = function(){ svf_50m_16d_200r<-svf(raster50m, nAngles = 16, maxDist = 200, ll=F)
#    writeRaster(svf_50m_16d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_200r"))
#  },
#  svf_50m_16d_400r = function() {svf_50m_16d_400r<-svf(raster50m, nAngles = 16, maxDist = 400, ll=F)
#    writeRaster(svf_50m_16d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_400r"))
#  },
  svf_50m_16d_800r = function() {svf_50m_16d_800r<-svf(raster50m, nAngles = 16, maxDist = 800, ll=F)
    writeRaster(svf_50m_16d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_16d_800r"))
  },
#  
#  
# 
#  
#  svf_50m_32d_5r = function() {svf_50m_32d_5r<-svf(raster50m, nAngles = 32, maxDist = 5, ll=F)
#  writeRaster(svf_50m_32d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_5r"))
#  },
#  svf_50m_32d_10r = function() {svf_50m_32d_10r<-svf(raster50m, nAngles = 32, maxDist = 10, ll=F)
#  writeRaster(svf_50m_32d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_10r"))
#  },
#  svf_50m_32d_25r = function() { svf_50m_32d_25r <- svf(raster50m, nAngles = 32, maxDist = 25, ll=F)
#  writeRaster(svf_50m_32d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_25r"))
#  },
#  svf_50m_32d_50r = function() {svf_50m_32d_50r<-svf(raster50m, nAngles = 32, maxDist = 50, ll=F)
#  writeRaster(svf_50m_32d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_50r"))
#  },
  svf_50m_32d_100r = function() {svf_50m_32d_100r<-svf(raster50m, nAngles = 32, maxDist = 100, ll=F)
  writeRaster(svf_50m_32d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_100r"))
  }
#  svf_50m_32d_200r = function(){ svf_50m_32d_200r<-svf(raster50m, nAngles = 32, maxDist = 200, ll=F)
#  writeRaster(svf_50m_32d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_200r"))
#  },
#  svf_50m_32d_400r = function() {svf_50m_32d_400r<-svf(raster50m, nAngles = 32, maxDist = 400, ll=F)
#  writeRaster(svf_50m_32d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_400r"))
#  },
#  svf_50m_32d_800r = function() {svf_50m_32d_800r<-svf(raster50m, nAngles = 32, maxDist = 800, ll=F)
#  writeRaster(svf_50m_32d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_32d_800r"))
#  },
#  
#  
#  
#  
#  
#  svf_50m_64d_5r = function() {svf_50m_64d_5r<-svf(raster50m, nAngles = 64, maxDist = 5, ll=F)
#  writeRaster(svf_50m_64d_5r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_5r"))
#  },
#  svf_50m_64d_10r = function() {svf_50m_64d_10r<-svf(raster50m, nAngles = 64, maxDist = 10, ll=F)
#  writeRaster(svf_50m_64d_10r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_10r"))
#  },
#  svf_50m_64d_25r = function() { svf_50m_64d_25r <- svf(raster50m, nAngles = 64, maxDist = 25, ll=F)
#  writeRaster(svf_50m_64d_25r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_25r"))
#  },
#  svf_50m_64d_50r = function() {svf_50m_64d_50r<-svf(raster50m, nAngles = 64, maxDist = 50, ll=F)
#  writeRaster(svf_50m_64d_50r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_50r"))
#  },
#  svf_50m_64d_100r = function() {svf_50m_64d_100r<-svf(raster50m, nAngles = 64, maxDist = 100, ll=F)
#  writeRaster(svf_50m_64d_100r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_100r"))
#  },
#  svf_50m_64d_200r = function(){ svf_50m_64d_200r<-svf(raster50m, nAngles = 64, maxDist = 200, ll=F)
#  writeRaster(svf_50m_64d_200r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_200r"))
#  },
#  svf_50m_64d_400r = function() {svf_50m_64d_400r<-svf(raster50m, nAngles = 64, maxDist = 400, ll=F)
#  writeRaster(svf_50m_64d_400r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_400r"))
#  },
#  svf_50m_64d_800r = function() {svf_50m_64d_800r<-svf(raster50m, nAngles = 64, maxDist = 800, ll=F)
#  writeRaster(svf_50m_64d_800r, paste0(sensAnalysisOutput_dir,"svf_50m_64d_800r"))
#  }
#  



)

  

# Using fork()
out <- mclapply( 
  tasks, 
  function(f) f(), 
  mc.cores = 48 
)

#for(i in 1:length(tasks)){
#  writeRaster(out[[i]],paste0("/home/pagani/development/outputTilesTest/sensAnalysisHorizonV12/",names(tasks[i])))
#}


  
  
  
  
