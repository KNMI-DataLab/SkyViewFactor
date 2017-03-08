library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)



SVF<-function(){

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

#registerDoParallel(2)


lazFolder = "/run/media/pagani/knmi/ahn2_clean/tileslaz"


pointX<- 244001
pointY<-576001


tileNumberXCoord<-floor(pointX/1000)*1000
tileNumberYCoord<-floor(pointY/1000)*1000

maxView<-100
mainTile<-loadTile(lazFolder, tileNumberXCoord, tileNumberYCoord)
mainTile<-makeSpatialDF(mainTile,projection = pro)
extensionMainTile<-extent(mainTile)

loadNeighborTiles(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)

fileToLoad<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord,".laz")
tileNeighborsRight<-c(paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz"),paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord,".laz"), paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord+1000,".laz"))
tileNeighborsLeft<-c(paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz"),paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz"), paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz"))
tileNeighborsUpDown<-c(paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz"),paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz"))

filesToLoad<-c(tileNeighborsLeft,tileNeighborsRight,tileNeighborsUpDown,fileToLoad)

paths<-list()
for(file in filesToLoad){
  
  print(file)
  fileLoc<-list.files(path = "/run/media/pagani/knmi/ahn2_clean/tileslaz", pattern = file,recursive = T, full.names = T)    
  paths<-c(paths,fileLoc)
}

print(paths)



#bash commands
#system("echo $AHN")
#system("cp $AHN /nobackup/users/dirksen/SVF_highres/SVF/test/LAS/$AHNname.laz")

## way to unzip them all
lapply(paths,file.copy,to="data/tiles/")
currentFiles<-list.files(path = "data/tiles/", full.names = TRUE)
lapply(paste("/usr/people/pagani/opt/testFile/LAStools/bin/laszip", currentFiles), system)
#system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
system("rm data/tiles/*.laz")

file_las<-list.files("data/tiles",pattern="*.las$",full.names = TRUE)
# file_las<-file_las[1]

centralFile<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord,".las")

centralFilePath<- list.files("data/tiles",pattern=centralFile, full.names=TRUE)

centralTile<-readLAS(centralFilePath)
centralTile<-data.frame(centralTile)

coordinates(centralTile)<-~X+Y
proj4string(centralTile)<-pro

maxView<-100

centralTileInfo<-extent(centralTile)

out.matrix<-lapply(file_las, readLAS)
out<-do.call(rbind.data.frame, out.matrix)
#out<-data.frame(out.matrix)

rm(out.matrix)
coordinates(out)<-~X+Y
proj4string(out)<-pro

xres<-5 # x-resolution in meters
yres<-5 # y-resolution in meters

r_test<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
extent(r_test)<-extent( centralTile)+2*maxView #changed to the extent of the central tile plus the radius # same extent as the las file
res(r_test)<-c(xres,yres) # set the resolution

out<-crop(out,extent(r_test))

r<-rasterize(out,r_test,field="Z") #rasterizing the spatial points to a 1x1 grid

rm(out)

###
r.AHN3<-r

###
r.svf<-svf(r, nAngles=16, maxDist= maxView, ll=F)

##reducing the grid to just the central part after computing the svf with the extra bound
extent(r.svf)<-extent(centralTile)
extent(r)<-extent(centralTile)

# plot(r.svf)
r.b<-brick(r,r.svf)
names(r.b)<-c("Z","SVF")
r.df<-as.data.frame(r.b,xy=TRUE)

r.df2<-r.df[complete.cases(r.df),]

cells<-ncell(r.svf)
write.table(cells,file="/nobackup/users/pagani/cells.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
writeRaster(r.b,filename=paste0(GRD,filename),format="raster")
write.table(r.df,file="testSVF.txt",sep=",",row.names = FALSE)
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
  lapply(paste("/usr/people/pagani/opt/testFile/LAStools/bin/laszip", currentFiles), system)
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



