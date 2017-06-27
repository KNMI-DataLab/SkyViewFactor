#' Load tile whole Netherlands
#' @title Load Tiles
#' @description Load tiles for the whole Netherlands
#' @param filepath path to the LAS files
#' @export 

loadTileWholeNL <- function(filepath){
  
  #coordX<-str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  #coordY<-str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  
  file<-basename(filepath)
  splits<-unlist(strsplit(file, c("\\.")))
  splits<-unlist(strsplit(splits[[1]], "_"))
  
  
  coordX<-splits[[2]] #str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-splits[[3]] #str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  
  
  uuid<-UUIDgenerate()
  multifileFlag <- FALSE
  #multifileFlag<-checkIfMultiTile(path, coordX, coordY) not needed in this verison since file is already available
  if (length(splits) > 3)
  {
    multifileFlag <- TRUE
  }
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = lazFolder, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  dir.create(paste0(temp_dir,uuid,"/"))
  centralFile<-filepath #list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
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
  
  rm(out.matrix,multifiles,uuid,centralFile,files,currentFiles,files_las, splits, filepath)
  gc()
  return(outDF)
  }
  else {
    return(NULL)
  }

}