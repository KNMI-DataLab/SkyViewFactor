calculate_path<-function(theta,radius=radius,x=x,y=y,distance=distance){
  dx<-cos(theta)*radius
  dy<-sin(theta)*radius
  
  x1<-x+dx
  y1<-y+dy
  
  xy1<-data.frame(x1,y1)
  
  XY<-rbind(c(x,y),xy1)
  XY<-cbind(XY,distance)
  XY<-data.frame(XY)
  
  values<-raster::extract(buffer,XY[1:2])
  XY<-cbind(XY,values)
  names(XY)<-c("x","y","distance","height")
  return(XY)
}