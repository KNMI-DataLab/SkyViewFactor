loadNeighborTile_v2<-function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection,xadd=0,yadd=0){
  
  tileNeighborsLeftLowerX <- tileNumberXCoord+xadd
  tileNeighborsLeftLowerY <- tileNumberYCoord+yadd
  
  df <- loadTile(path,tileNeighborsLeftLowerX , tileNeighborsLeftLowerY)
  
  if(is.null(df) == FALSE){
    spdf  <- makeSpatialDF(df,projection)
    spdf1 <- crop(spdf,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  }
  if(exists("spdf1") == FALSE){
    spdf1<-NULL
  }
  return(spdf1)
}