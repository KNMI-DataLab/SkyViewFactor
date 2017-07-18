#' Load a tile into memory from a LAZ file and clean up the disk location after is loaded in memory
#' 
#' \code{loadTile}  loads a tile into memory from a LAZ file given the path where the tiles are and the coordinates.
#' A check is performed if a tile is composed by multiple tiles; if so they are loaded as well.
#' 
#' @seealso This function is called in \code{\link{mergeNeighborTiles}}
#' @param path filesystem location of all tiles
#' @param coordX x-coordinates in meters (RDcoordinates)
#' @param coordY y-coordinates in meters (RDcoordinates)
#' 
#' 
#' 
#' @export


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
  system(paste0("rm -r ", temp_dir,uuid))
  rm(out.matrix,multifiles,uuid,centralFile,files,currentFiles,files_las)
  gc()
  return(outDF)
  }
  else {
    return(NULL)
  }

}