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



listGRDTiles <- list.files(path = raster_dir_bird, pattern = ".grd", full.names = T, recursive = T)

#listGRDTiles<-list(listGRDTiles)
#commented out for the single test of the timing
test<-lapply(listGRDTiles,raster)
merged <- do.call(merge, test)



raster2m<-aggregate(merged,fact=2)
raster3m<-aggregate(merged,fact=3)
raster4m<-aggregate(merged,fact=4)
raster10m<-aggregate(merged,fact=10)
raster20m<-aggregate(merged,fact=20)
raster50m<-aggregate(merged,fact=50)




tasks<-list(
svf100_2m = function() svf(raster2m, nAngles = 16, maxDist = 100, ll=F),
svf100_3m = function() svf(raster3m, nAngles = 16, maxDist = 100, ll=F),
svf100_4m = function() svf(raster4m, nAngles = 16, maxDist = 100, ll=F),
svf100_10m = function() svf(raster10m, nAngles = 16, maxDist = 100, ll=F),
svf100_20m = function() svf(raster20m, nAngles = 16, maxDist = 100, ll=F),
svf100_50m = function() svf(raster50m, nAngles = 16, maxDist = 100, ll=F)
)

  

# Using fork()
out <- mclapply( 
  tasks, 
  function(f) f(), 
  mc.cores = 8 
)

for(i in 1:length(tasks)){
  writeRaster(out[[i]],paste0("/home/pagani/development/outputTilesTest/100mRsensRes/",names(tasks[i])))
}


  
  
  
  
