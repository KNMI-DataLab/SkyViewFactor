library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)


pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

registerDoParallel(2)


lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "~/tools/LAStools/bin/laszip"

xres<<-5 # x-resolution in meters
yres<<-5 # y-resolution in meters

maxView<<-100


pointX<- 244001
pointY<-576001

c1<-c(pointX, pointY)

pointX2<- 121490
pointY2<- 487040

c2<-c(pointX2, pointY2)

coord <- list(c1, c2)


foreach(i = 1:length(coord)) %dopar%
{
  SVF(coord[i][1], coord[i][2],maxView, pro)
}



loadTile <- function(path, coordX, coordY){
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  lapply(files,file.copy,to="data/tiles/")
  currentFiles<-list.files(path = "data/tiles/", full.names = TRUE)
  lapply(paste(lasZipLocation, currentFiles), system)
  #system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
  system("rm data/tiles/*.laz")
  files_las<-list.files("data/tiles",pattern="*.las$",full.names = TRUE)
  out.matrix<-lapply(files_las, readLAS)
  outDF<-do.call(rbind.data.frame, out.matrix)
  #out<-data.frame(out.matrix)
  system("rm data/tiles/*.las")
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
  
  
  tileNumberXCoord<-floor(pointX/1000)*1000
  tileNumberYCoord<-floor(pointY/1000)*1000
  
  
  mainTile<-loadTile(lazFolder, tileNumberXCoord, tileNumberYCoord)
  mainTile<-makeSpatialDF(mainTile,projection = pro)
  extensionMainTile<-extent(mainTile)
  
  neighbors<-loadNeighborTiles(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  #neighbors<-loadNeighborTilesTest(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  
  

  
  
  
  rasterizedNeighbors<-lapply(neighbors, makeRaster, xres, yres, pro)
  mergedNeighbors<-do.call(merge, rasterizedNeighbors)
  rasterizedMainTile<-makeRaster(mainTile,xres,yres,pro)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors)
  
  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))
  
  
  # plot(r.svf)
  r.b<-brick(rasterizedMainTile,r.svf)
  names(r.b)<-c("Z","SVF")
  r.df<-as.data.frame(r.b,xy=TRUE)
  
  r.df2<-r.df[complete.cases(r.df),]
  
  #cells<-ncell(r.svf)
  #write.table(cells,file="/nobackup/users/pagani/cells.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
  #writeRaster(r.b,filename=paste0(GRD,filename),format="raster")
  write.table(r.df,file="testSVF.txt",sep=",",row.names = FALSE)
}




