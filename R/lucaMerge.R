library(data.table)
library(rgdal)
library(stringr)
library(raster)


getTileNumber <- function(filepath){
  
  
  file<-basename(filepath)
  splits<-unlist(strsplit(file, c("\\.")))
  splits<-unlist(strsplit(splits[[1]], "_"))
  
  
  coordX<-splits[[2]] #str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-splits[[3]]
  
  coordsXY<-c(str_pad(as.integer(coordX), width = 6, pad = "0"), str_pad(as.integer(coordY), width = 6, pad = "0"))
  rm(file,splits,coordX,coordY)
  return(coordsXY)
  
}

extractSVFfromFile<-function(X,Y){
  coordXFile<-str_pad(as.integer(floor(X/1000)*1000), 6, pad = "0")
  coordYFile<-str_pad(as.integer(floor(Y/1000)*1000), 6, pad = "0")
  rasterSVF<-brick(paste0("~/SVFEindhoven/",coordXFile,"_",coordYFile))
  testDF<-data.frame(X,Y)
  coordinates(testDF)<-~X+Y
  svf<-extract(rasterSVF$SVF,testDF,proj4string = proRDH)
  svf
}

getNeighbors<-function(X,Y){
  Xleft<-X-1000
  Xright<-X+1000
  Ybelow<-Y-1000
  Yabove<-Y+1000
  cells<-expand.grid(c(X,Xleft,Xright),c(Y,Yabove,Ybelow))
  print(cells)
  
  rastersCells<-apply(cells[,c('Var1','Var2')], 1, function(x) getRaster(x[1],x[2]))
  mergedRaster<-do.call(raster::merge,c(rastersCells, tolerance =100))
  
  
  
}


getRaster<-function(X,Y){
  coordXFile<-str_pad(as.integer(floor(X/1000)*1000), 6, pad = "0")
  coordYFile<-str_pad(as.integer(floor(Y/1000)*1000), 6, pad = "0")
  multifileFlag<-checkIfMultiTile(path,X,Y)
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_",X,"_", Y,"_.*"), full.names = T, recursive = T)
    rasterData<-lapply(multifiles, function(i){stack(i)})
  } else{
    multifiles<-file
    rasterData<-lapply(multifiles,stack)
  }
  
  
  
  
  merrasters<-do.call(raster::merge,c(rasterData, tolerance =100))
  merrasters
}


checkIfMultiTile <- function(path, coordX, coordY){
  multifile<-FALSE
  file<-list.files(path = path, pattern = paste0("ahn_", coordX,"_",coordY,"_1.*"), full.names = T, recursive = T)
  if (length(file)!=0){
    multifile<-TRUE}
  rm(file)
  gc()
  return(multifile)
}  


path<<-"/data1/"
file<-"/data1/lidarTilesGTiff_1m/ahn_109000_410000_1.tif"
coords<-getTileNumber(file)
multifileFlag<-checkIfMultiTile(path,coords[[1]],coords[[2]])
coordX<-as.numeric(coords[[1]])
coordY<-as.numeric(coords[[2]])
if(multifileFlag){
  multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
} else{
  multifiles<-file
}
neig<-getNeighbors(coordX,coordY)
#mergedNeighbors<-do.call(merge, c(rasterizedNeighbors, tolerance =10))




