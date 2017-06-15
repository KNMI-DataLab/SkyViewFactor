#' Check neighboring tiles
#' @title Check multitile
#' @param path location to the files
#' @param coordX x-coord name of the file, rounded 6 numbered RD-coord
#' @param coordY y-coord name of the file, rounded 6 numbered RD-coord
#' @export

checkIfMultiTile <- function(path, coordX, coordY){
  multifile<-FALSE
  file<-list.files(path = path, pattern = paste0("ahn_", coordX,"_",coordY,"_1.*"), full.names = T, recursive = T)
  if (length(file)!=0){
    multifile<-TRUE}
  rm(file)
  gc()
  return(multifile)
}  