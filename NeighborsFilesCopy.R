getNeighborAndMainTilesFile <- function(path,tileNumberXCoord, tileNumberYCoord){
  
  
  shiftx<-c(0,1000,-1000)
  shifty<-c(0,1000,-1000)
  totalFiles<-NULL
  
  for(i in shiftx){
    for(j in shifty){
  
  
  #print("hello")
  #tileNeighborsLeftLower<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz")
  x<-as.numeric(tileNumberXCoord)+i
  y<-as.numeric(tileNumberYCoord)+j
  
  x<-str_pad(as.integer(floor(x/1000)*1000), 6, pad = "0")
  y<-str_pad(as.integer(floor(y/1000)*1000), 6, pad = "0")
  
  flag<-checkIfMultiTile(path,x,y)
  if(flag){
  file_s<-list.files(path = lazFolder, pattern = paste0("ahn_", x,"_", y,"_.*"), full.names = T, recursive = T)
} else {
  file_s<-list.files(path = lazFolder, pattern = paste0("ahn_", x,"_", y,".*"), full.names = T, recursive = T)
}
  totalFiles <- c(totalFiles,file_s)
    }
  }
  
totalFiles}