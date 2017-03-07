library(shiny)
library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)

function(input,output,session){
############################################
############################################
############################################
#Define all the input variables 

#fixed variables
pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

#dynamic variables
pointX  <- input$pointX  # x-coordinates of a point
pointY  <- input$pointY  # y-cooridnates of a point
xres    <- input$xres    # x-resolution in meters for the SVF
yres    <- input$yres    # y-resolution in meters for the SVF
maxView <- input$maxView # search radius for the SVF
angles  <- input$angles  # number of angles for the SVF
############################################
############################################
############################################

## The rest of the script
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
## way to unzip them all
lapply(paths,file.copy,to="data/tiles/")
currentFiles<-list.files(path = "data/tiles/", full.names = TRUE)
lapply(paste("/usr/people/pagani/opt/testFile/LAStools/bin/laszip", currentFiles), system)
system("rm data/tiles/*.laz")

file_las<-list.files("data/tiles",pattern="*.las$",full.names = TRUE)
centralFile<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord,".las")

centralFilePath<- list.files("data/tiles",pattern=centralFile, full.names=TRUE)

#From LAS to spatial object
centralTile<-readLAS(centralFilePath)
centralTile<-data.frame(centralTile)
coordinates(centralTile)<-~X+Y
proj4string(centralTile)<-pro

centralTileInfo<-extent(centralTile)
out.matrix<-lapply(file_las, readLAS)
out<-do.call(rbind.data.frame, out.matrix)
rm(out.matrix)
coordinates(out)<-~X+Y
proj4string(out)<-pro

#Dummy Raster
r_test<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
extent(r_test)<-extent( centralTile)+2*maxView #changed to the extent of the central tile plus the radius # same extent as the las file
res(r_test)<-c(xres,yres) # set the resolution
out<-crop(out,extent(r_test))
r<-rasterize(out,r_test,field="Z") #rasterizing the spatial points to a 1x1 grid
rm(out)
#####################################
#####################################
#Calculate the SVF
r.svf<-svf(r, nAngles=angles, maxDist= maxView, ll=F)

##reducing the grid to just the central part after computing the svf with the extra bound
extent(r.svf)<-extent(centralTile)
extent(r)<-extent(centralTile)
r.b<-brick(r,r.svf)
names(r.b)<-c("Z","SVF")
#####################################
#####################################

#For the plot output
output$plot1 <-renderPlot({
plot(r.b,main=paste0("Sky View Factor \n xres=",xres,"\n yres=",yres,"\n radius=",maxView))
  
})  

}
