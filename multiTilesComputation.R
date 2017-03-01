library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

registerDoParallel(2)


pointX<- 140874
pointY<-457916


tileNumberXCoord<-floor(pointX/1000)*1000
tileNumberYCoord<-floor(pointY/1000)*1000

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
coordinates(out)<-~X+Y
proj4string(out)<-pro

xres<-5 # x-resolution in meters
yres<-5 # y-resolution in meters

r_test<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
extent(r_test)<-extent( centralTile)+2*maxView #changed to the extent of the central tile plus the radius # same extent as the las file
res(r_test)<-c(xres,yres) # set the resolution

r<-rasterize(out,r_test,field="Z") #rasterizing the spatial points to a 1x1 grid

###
r.AHN3<-r

###
r.svf<-svf(r, nAngles=16, maxDist= maxView, ll=F)
# plot(r.svf)
r.b<-brick(r,r.svf)
names(r.b)<-c("Z","SVF")
r.df<-as.data.frame(r.b,xy=TRUE)

r.df2<-r.df[complete.cases(r.df),]

cells<-ncell(r.svf)
write.table(cells,file="/nobackup/users/pagani/cells.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
writeRaster(r.b,filename=paste0(GRD,filename),format="raster")
write.table(r.df,file="testSVF.txt"),sep=",",row.names = FALSE)
