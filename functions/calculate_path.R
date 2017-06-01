calculate_path<-function(theta,x=x,y=y,distance=distance){
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
  
  values<-raster::extract(buffer,XY[1:2])
  XY<-cbind(XY,values)
  names(XY)<-c("x","y","distance","height")
  return(XY)
}