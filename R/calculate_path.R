#' Calculate paths from a raster
#' 
#' @title Calculate Path
#' \code{calculate_path} calculates the path for given distance, angles and coordinates from a raster file.
#' 
#' @description calculates the path for different angles
#' @seealso This function is called in \code{\link{lines_from_las}}
#' @param theta angle or angles in degrees
#' @param x x-coordinates in meters (RDcoordinates)
#' @param y y-coordinates in meters (RDcoordinates)
#' @param distance distance for which the path should be calculated
#' @param rbuffer raster which is masked in \code{\link{lines_from_las}}
#' 
#' 
#' @export

calculate_path<-function(theta,x=x,y=y,distance,rbuffer){
  dx<-cos(theta)*distance
  dy<-sin(theta)*distance
  
  x1<-x+dx
  y1<-y+dy
  
  xy1<-data.frame(x1,y1)
  
  if(length(xy1$x1)!=length(distance)){
    warning('Your vectors have different lengths, returning NULL')
    return(NULL)
  }
  #XY<-rbind(c(x,y),xy1) #Bug!
  XY<-cbind(xy1,distance)
  XY<-data.frame(XY)
  
  values<-raster::extract(rbuffer,XY[1:2],buffer=3,fun=mean,na.rm=TRUE)
  XY<-cbind(XY,values)
  names(XY)<-c("x","y","distance","height")
  return(XY)
}