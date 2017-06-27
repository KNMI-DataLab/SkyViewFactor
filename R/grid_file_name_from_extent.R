#' Grid file names from extent
#' @title Grid file names
#' @description get the grid file names for an extention 
#' @param ext extent in RD-coordinates
#' @export

grid_file_name_from_extent<-function(ext=Haag.ext){
  
  
  topleft<-SpatialPoints(cbind(ext[1],ext[4]), proj4string=pro)
  topright<-SpatialPoints(cbind(ext[2],ext[4]), proj4string=pro)
  downleft<-SpatialPoints(cbind(ext[1],ext[3]), proj4string=pro)
  downright<-SpatialPoints(cbind(ext[2],ext[3])  , proj4string=pro)
  
  diff.top<-topright@coords-topleft@coords
  xdiff<-diff.top[1]
  xnr.tiles<-ceiling(xdiff/1000)
  
  diff.bottom<-topright@coords-downright@coords
  ydiff<-diff.bottom[2]
  ynr.tiles<-ceiling(ydiff/1000)
  
  #Get the first tile (topleft)
  tl<-data.frame(topleft)
  names(tl)<-c("x","y")
  tileNumberXCoord<-floor(tl$x/1000)*1000
  toprow.xtiles<-seq(tileNumberXCoord,tileNumberXCoord+xnr.tiles*1000,by=1000)
  
  bl<-data.frame(downleft)
  names(bl)<-c("x","y")
  tileNumberYCoord<-floor(bl$y/1000)*1000
  downrow.ytiles<-seq(tileNumberYCoord,tileNumberYCoord+ynr.tiles*1000,by=1000)
  
  tiles.out<-data.frame(expand.grid(toprow.xtiles,downrow.ytiles))
  tiles.out<-apply(tiles.out,1,function(x) paste0(str_pad(as.integer(x[1]), width = 6, pad = "0"),"_",
                                                  str_pad(as.integer(x[2]),  width = 6, pad = "0"), ".grd"))
  return(tiles.out)
}
#Test how the function workd
#test <- c("A","B","C","D")
#test2<-c("E","F","G","H")
#expand.grid(test,test2)

#tiles_unique<-unique(point[c("tileNumberXCoord","tileNumberYCoord")])
#tiles_unique_names<-paste0(str_pad(as.integer(tiles_unique$tileNumberXCoord), width = 6, pad = "0"),"_",
#                           str_pad(as.integer(tiles_unique$tileNumberYCoord),  width = 6, pad = "0"), ".grd")  
#plot(Haag.r)
#points(topleft,col="red")
#points(topright,col="blue")
#points(downright,col="orange")
#points(downleft,col="green")