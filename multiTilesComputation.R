library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(uuid)
library(data.table)

pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

GMS_meta<-fread("GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)

registerDoParallel(7)

workingPath <<- getwd()


lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"

Xres<<-5 # x-resolution in meters
Yres<<-5 # y-resolution in meters

maxView<<-100


#pointX<- 244001
#pointY<-576001

#c1<-c(pointX, pointY)
coordsGMS<-as(GMS_meta,"SpatialPoints")
coordsGMS<-data.frame(coordsGMS)

coordsGMS$tileNumberXCoord<-floor(coordsGMS$loc_lon/1000)*1000
coordsGMS$tileNumberYCoord<-floor(coordsGMS$loc_lat/1000)*1000

tiles_unique<-unique(coordsGMS[c("tileNumberXCoord","tileNumberYCoord")])

pointX2<- 121490
pointY2<- 487040

c2<-c(pointX2, pointY2)

coord <<- list( c2)

dir.create("/home/pagani/development/SkyViewFactor/data/tiles")

system.time(
foreach(i =  25:length(coordsGMS[,1]), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"), 
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "maxView", "Xres", "Yres", "coord")) %do%
{
  print(i)
  SVF(coordsGMS[i,]$loc_lon, coordsGMS[i,]$loc_lat,maxView, pro)
  #SVF(coord[i,]$loc_lon, coord[i,]$loc_lat,maxView, pro)
}

)
#26 rasters were computed without error, checking the 27th and 28th file
SVF(133743.9, 445509.3,maxView, pro)

unlink("/home/pagani/development/SkyViewFactor/data/tiles/", recursive = T)






##############################


allTiles<-list.files(path = lazFolder, "*.laz", full.names = T, recursive = T)

loadTile <- function(path, coordX, coordY){
  uuid<-UUIDgenerate()
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  dir.create(paste0(workingPath,"/data/tiles/",uuid,"/"))
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  lapply(files,file.copy,to=paste0(workingPath,"/data/tiles/",uuid,"/"))
  currentFiles<-list.files(path = paste0(workingPath,"/data/tiles/", uuid,"/"), full.names = TRUE)
  lapply(paste(lasZipLocation, currentFiles), system)
  #system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
  system(paste0("rm ", workingPath, "/data/tiles/", uuid,"/*.laz"))
  files_las<-list.files(paste0(workingPath, "/data/tiles/", uuid),pattern="*.las$",full.names = TRUE)
  out.matrix<-lapply(files_las, readLAS)
  outDF<-do.call(rbind.data.frame, out.matrix)
  #out<-data.frame(out.matrix)
  system(paste0("rm ", workingPath, "/data/tiles/", uuid,"/*.las"))
  rm(out.matrix)
  outDF
}



checkIfMultiTile <- function(path, coordX, coordY){
  multifile<-FALSE
  file<-list.files(path = path, pattern = paste0("ahn_", coordX,"_",coordY,"_1.*"), full.names = T, recursive = T)
  if (length(file)!=0){
    multifile<-TRUE}
  multifile 
}  

##when the input is  a tile and not a coord pair
# checkIfMultiTileInputTile <- function(lazfilepath){
#   multifile<-FALSE
#   pathSplit<-strsplit(lazfilepath,split = "ahn",)[[1]]
#   filename<-paste0("ahn",pathSplit[[length(filename)]])
#   folder<-strsplit(pathSplit[[1]])
#   rootFilename <- strsplit(filename, split = ".")[[1]]
#   rootFilename<-rootFilename[[1]]
#   
#   
#   file<-list.files(path = folder, pattern = paste0(rootFilename,"_1.*"), full.names = T, recursive = T)
#   if (length(file)!=0){
#     multifile<-TRUE}
#   multifile 
# }  

makeSpatialDF <- function(df,projection){
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  df
}

loadNeighborTiles <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
  print("hello")
  #tileNeighborsLeftLower<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsLeftLowerX<-tileNumberXCoord-1000
  tileNeighborsLeftLowerY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsLeftLowerX,tileNeighborsLeftLowerY)
  df<-makeSpatialDF(df,projection)
  df1<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))

  #tileNeighborLeft<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord,".laz")
  tileNeighborsLeftX<-tileNumberXCoord-1000
  tileNeighborsLeftY<-tileNumberYCoord
  df<-loadTile(path,tileNeighborsLeftX,tileNeighborsLeftY)
  df<-makeSpatialDF(df,projection)
  df2<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile), ymax(extensionMainTile)))

  #tileNeighborLeftUpper<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsLeftUpperX<-tileNumberXCoord-1000
  tileNeighborsLeftUpperY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsLeftUpperX,tileNeighborsLeftUpperY)
  df<-makeSpatialDF(df,projection)
  df3<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))

  #tileNeighborsRightLower<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsRightLowerX<-tileNumberXCoord+1000
  tileNeighborsRightLowerY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsRightLowerX,tileNeighborsRightLowerY)
  df<-makeSpatialDF(df,projection)
  df4<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))

  #tileNeighborRight<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz")
  tileNeighborsRightX<-tileNumberXCoord+1000
  tileNeighborsRightY<-tileNumberYCoord
  df<-loadTile(path,tileNeighborsRightX,tileNeighborsRightY)
  df<-makeSpatialDF(df,projection)
  df5<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile), ymax(extensionMainTile)))

  #tileNeighborRightUpper<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsRightUpperX<-tileNumberXCoord+1000
  tileNeighborsRightUpperY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsRightUpperX,tileNeighborsRightUpperY)
  df<-makeSpatialDF(df,projection)
  df6<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymax(extensionMainTile), ymax(extensionMainTile)+maxView))

  #tileNeighborsCentralDown<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsCenterDownX<-tileNumberXCoord
  tileNeighborsCenterDownY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsCenterDownX,tileNeighborsCenterDownY)
  df<-makeSpatialDF(df,projection)
  df7<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))

  #tileNeighborsCentralUp<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsCenterUpX<-tileNumberXCoord
  tileNeighborsCenterUpY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsCenterUpX,tileNeighborsCenterUpY)
  df<-makeSpatialDF(df,projection)
  df8<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  
  dfs<-c(df1,df2,df3,df4,df5,df6,df7,df8)
}



loadNeighborTilesTest <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
  print("hello")
  #tileNeighborsLeftLower<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsLeftLowerX<-tileNumberXCoord-1000
  tileNeighborsLeftLowerY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsLeftLowerX,tileNeighborsLeftLowerY)
  df<-makeSpatialDF(df,projection)
  df1<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  
  #tileNeighborLeft<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord,".laz")
  tileNeighborsLeftX<-tileNumberXCoord-1000
  tileNeighborsLeftY<-tileNumberYCoord
  df<-loadTile(path,tileNeighborsLeftX,tileNeighborsLeftY)
  df<-makeSpatialDF(df,projection)
  df2<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile), ymax(extensionMainTile)))
  
  # #tileNeighborLeftUpper<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord+1000,".laz")
  # tileNeighborsLeftUpperX<-tileNumberXCoord-1000
  # tileNeighborsLeftUpperY<-tileNumberYCoord+1000
  # df<-loadTile(path,tileNeighborsLeftUpperX,tileNeighborsLeftUpperY)
  # df<-makeSpatialDF(df,projection)
  # df3<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  # 
  # #tileNeighborsRightLower<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz")
  # tileNeighborsRightLowerX<-tileNumberXCoord+1000
  # tileNeighborsRightLowerY<-tileNumberYCoord-1000
  # df<-loadTile(path,tileNeighborsRightLowerX,tileNeighborsRightLowerY)
  # df<-makeSpatialDF(df,projection)
  # df4<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  # 
  # #tileNeighborRight<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz")
  # tileNeighborsRightX<-tileNumberXCoord+1000
  # tileNeighborsRightY<-tileNumberYCoord
  # df<-loadTile(path,tileNeighborsRightX,tileNeighborsRightY)
  # df<-makeSpatialDF(df,projection)
  # df5<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile), ymax(extensionMainTile)))
  # 
  # #tileNeighborRightUpper<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz")
  # tileNeighborsRightUpperX<-tileNumberXCoord+1000
  # tileNeighborsRightUpperY<-tileNumberYCoord+1000
  # df<-loadTile(path,tileNeighborsRightUpperX,tileNeighborsRightUpperY)
  # df<-makeSpatialDF(df,projection)
  # df6<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  # 
  # #tileNeighborsCentralDown<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz")
  # tileNeighborsCenterDownX<-tileNumberXCoord
  # tileNeighborsCenterDownY<-tileNumberYCoord-1000
  # df<-loadTile(path,tileNeighborsCenterDownX,tileNeighborsCenterDownY)
  # df<-makeSpatialDF(df,projection)
  # df7<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  # 
  # #tileNeighborsCentralUp<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz")
  # tileNeighborsCenterUpX<-tileNumberXCoord
  # tileNeighborsCenterUpY<-tileNumberYCoord+1000
  # df<-loadTile(path,tileNeighborsCenterUpX,tileNeighborsCenterUpY)
  # df<-makeSpatialDF(df,projection)
  # df8<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  # 
  dfs<-c(df1,df2)
}



makeRaster<-function(spatialDF, xres, yres, pro){
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(spatialDF)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(spatialDF,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid
  r
}



SVF<-function(pointX, pointY, maxView, proj){
  
  
  tileNumberXCoord<-str_pad(floor(pointX/1000)*1000, 6, pad ="0")
  tileNumberYCoord<-str_pad(floor(pointY/1000)*1000, 6, pad ="0")
  
  
  mainTile<-loadTile(lazFolder, tileNumberXCoord, tileNumberYCoord)
  mainTile<-makeSpatialDF(mainTile,projection = pro)
  extensionMainTile<-extent(mainTile)
  
  neighbors<-loadNeighborTiles(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  #neighbors<-loadNeighborTilesTest(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  
  rasterizedNeighbors<-lapply(neighbors, makeRaster, Xres, Yres, pro)
  mergedNeighbors<-do.call(merge, rasterizedNeighbors)
  rasterizedMainTile<-makeRaster(mainTile,Xres,Yres,pro)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors)
  
  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))
  
  
  # plot(r.svf)
  r.b<-brick(rasterizedMainTile,out)
  names(r.b)<-c("Z","SVF")
  r.df<-as.data.frame(r.b,xy=TRUE)
  
  #r.df2<-r.df[complete.cases(r.df),]
  
  #cells<-ncell(r.svf)
  #write.table(cells,file="/nobackup/users/pagani/cells.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
  writeRaster(r.b,filename=paste0("/home/pagani/development/SkyViewFactor/data/gridsSVF/",
                                  tileNumberXCoord, "_",tileNumberYCoord,".grd"),
                                  format="raster",
                                  overwrite=TRUE)
  #write.table(r.df,file="testSVF.txt",sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists("testSVF.txt"))
  
}




