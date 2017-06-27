#' Load a tile
#' @title Load LAS tile
#' @description loads the first tile where the point is located
#' @param path is the path to the files
#' @param coordX x-coordinate in RD coordinates
#' @param coordY y-coordinate in RD coordinates
#' @export
#' 
loadTile <- function(path, coordX, coordY){
  
  coordX<-str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  
  uuid<-UUIDgenerate()
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  dir.create(paste0(temp_dir,uuid,"/"))
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  if(length(files)!=0){
  lapply(files,file.copy,to=paste0(temp_dir,uuid,"/"))
  currentFiles<-list.files(path = paste0(temp_dir, uuid,"/"), full.names = TRUE)
  #print(paste(currentFiles))
  lapply(paste(lasZipLocation, currentFiles), system)
  #system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
  system(paste0("rm ", temp_dir, uuid,"/*.laz"))
  files_las<-list.files(paste0(temp_dir, uuid),pattern="*.las$",full.names = TRUE)
  out.matrix<-lapply(files_las, readLAS)
  outDF<-do.call(rbind.data.frame, out.matrix)
  #out<-data.frame(out.matrix)
  system(paste0("rm ", temp_dir, uuid,"/*.las"))
  
  rm(out.matrix,multifiles,uuid,centralFile,files,currentFiles,files_las)
  gc()
  return(outDF)
  }
  else {
    return(NULL)
  }

}