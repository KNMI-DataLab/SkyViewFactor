#coordinates box
#Den Haag  51.7-52.4 N, 3.8-4.9 E. 
#Eindhoven 51.1-51.9 N, 5-6 E.
library(R.utils)
library(magrittr)
library(stringr)

library(raster)
library(horizon)
library(rgdal)
#library(rLiDAR)
library(foreach)
library(doParallel)
#library(uuid)
library(data.table)
library(stringr)
library(spatial.tools)
library(sp)

library(parallel)


#sourceDirectory("~/development/SkyViewFactor/functions")






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


listGRDTiles <- list.files(path = raster_dir, ".grd", full.names = T, recursive = T)

#listGRDTiles<-list(listGRDTiles)
test<-lapply(listGRDTiles,raster)
merged <- do.call(merge, test)



tasks<-list(
svf50_16 = function() svf(merged, nAngles = 16, maxDist = 50, ll=F),
svf50_8 = function() svf(merged, nAngles = 8, maxDist = 50, ll=F),
svf100_8 = function() svf(merged, nAngles = 8, maxDist = 100, ll=F),
svf200_8 = function() svf(merged, nAngles = 8, maxDist = 200, ll=F),
svf400_8 = function() svf(merged, nAngles = 8, maxDist = 400, ll=F),
svf800_8 = function() svf(merged, nAngles = 8, maxDist = 800, ll=F),
svf50_4 = function() svf(merged, nAngles = 4, maxDist = 50, ll=F),
svf100_4 = function() svf(merged, nAngles = 4, maxDist = 100, ll=F),
svf200_4 = function() svf(merged, nAngles = 4, maxDist = 200, ll=F),
svf400_4 = function() svf(merged, nAngles = 4, maxDist = 400, ll=F),
svf800_4 = function() svf(merged, nAngles = 4, maxDist = 800, ll=F),
svf50_3 = function() svf(merged, nAngles = 3, maxDist = 50, ll=F),
svf100_3 = function() svf(merged, nAngles = 3, maxDist = 100, ll=F),
svf200_3 = function() svf(merged, nAngles = 3, maxDist = 200, ll=F),
svf400_3 = function() svf(merged, nAngles = 3, maxDist = 400, ll=F),
svf800_3 = function() svf(merged, nAngles = 3, maxDist = 800, ll=F),
svf50_2 = function() svf(merged, nAngles = 2, maxDist = 50, ll=F),
svf100_2 = function() svf(merged, nAngles = 2, maxDist = 100, ll=F),
svf200_2 = function() svf(merged, nAngles = 2, maxDist = 200, ll=F),
svf400_2 = function() svf(merged, nAngles = 2, maxDist = 400, ll=F),
svf800_2 = function() svf(merged, nAngles = 2, maxDist = 800, ll=F),
svf50_1 = function() svf(merged, nAngles = 1, maxDist = 50, ll=F),
svf100_1 = function() svf(merged, nAngles = 1, maxDist = 100, ll=F),
svf200_1 = function() svf(merged, nAngles = 1, maxDist = 200, ll=F),
svf400_1 = function() svf(merged, nAngles = 1, maxDist = 400, ll=F),
svf800_1 = function() svf(merged, nAngles = 1, maxDist = 800, ll=F)
)

  

# Using fork()
out <- mclapply( 
  tasks, 
  function(f) f(), 
  mc.cores = 8 
)

for(i in 1:length(tasks)){
  writeRaster(out[[i]],paste0("/home/ubuntu/efs/output/1m/1m_",names(tasks[i])))
}


  
  
  
  
