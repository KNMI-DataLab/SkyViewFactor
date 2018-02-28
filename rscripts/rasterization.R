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


output_dir <- "/data1/lidarTilesGTiff_3m/"
lazFolder <- c("/data1", "/data2", "/data3")
lasZipLocation <- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
temp_dir <- "/home/pagani/development/SkyViewFactor/data/LAZsample/"


pro<-CRS("+init=epsg:28992")

registerDoParallel(6) #number of parallel cores


Xres<<-3 # x-resolution in meters
Yres<<-3 # y-resolution in meters

filesTilesInvolved<-list.files("/data1/lidarTilesDeBilt1m/",".grd")

lazInvolved<-gsub(".grd",".laz",filesTilesInvolved)

listTiles <- sapply(lazInvolved, function(x){list.files(path = lazFolder, x, full.names = T, recursive = T)},USE.NAMES = F)


foreach(i =  1:length(listTiles), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "temp_dir", "maxView", "Xres", "Yres",
                    "loadNeighborTile_v2","mergeNeighborTiles")) %dopar%
                    {
fileLAZ<-listTiles[[i]]
splitStr<-stringr::str_split(fileLAZ,"/", simplify = T)
fileLAS<-splitStr[[length(splitStr)]]
filename<-str_split(fileLAS,"\\.", simplify = T)[[1]]
fileLAS<-paste0(temp_dir,filename,".las")


if(!file.exists(paste0(output_dir, filename, ".tif")))
{
  system(paste0(lasZipLocation, " -i ", fileLAZ, " -o ", fileLAS))
  print(i)
  print(fileLAS)
  lasData<-readLAS(fileLAS)
  file.remove(fileLAS)
  DF<-data.frame(lasData)
  pro<-CRS("+init=epsg:28992")
  DFSpatial<-makeSpatialDF(DF,projection = pro)
  #Xres<-1
  #Yres<-1
  print(extent(DFSpatial))
  #introduced since several tiles have the x component with extention 0
  if((xmin(DFSpatial)!=xmax(DFSpatial)) & (ymin(DFSpatial)!=ymax(DFSpatial))){
  DFraster<-makeRaster(DFSpatial,Xres,Yres,pro)
  #writeRaster(DFraster,"/home/pagani/development/SkyViewFactor/data/LAZsample/testRaster")
  writeRaster(DFraster,paste0(output_dir,filename), format="GTiff")
  gc()
  }
}

}

