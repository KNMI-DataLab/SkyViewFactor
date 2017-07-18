#' Checks if a tile is made of multiple tiles with same root tile (e.g., ahn_264000_538000_1.laz 
#' ahn_264000_538000_2.laz ahn_264000_538000_3.laz ahn_264000_538000_4.laz)
#' 
#' \code{makeSpatialDF} returns TRUE if a tile is composed of multiple tiles with same root. 
#' 
#' @seealso This function is called in \code{\link{SVF}}
#' @param path location of the all tiles
#' @param coordX tile x coordinate (RDcoordinate)
#' @param coordY tile y coordinate (RDcoordinate)
#' @return multifile boolean, TRUE if multifile
#' 
#' 
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