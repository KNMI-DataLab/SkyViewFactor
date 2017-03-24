SVF<-function(pointX, pointY, maxView, proj){
  
  
  #tileNumberXCoord<-str_pad(as.integer(floor(pointX/1000)*1000), 6, pad = "0")
  #tileNumberYCoord<-str_pad(as.integer(floor(pointY/1000)*1000), 6, pad = "0")
  
  tileNumberXCoord<-pointX
  tileNumberYCoord<-pointY
  
  mainTile<-loadTile(lazFolder, as.integer(tileNumberXCoord), as.integer(tileNumberYCoord))
  mainTile<-makeSpatialDF(mainTile,projection = pro)
  extensionMainTile<-extent(mainTile)
  
  neighbors<-mergeNeighborTiles(lazFolder, as.integer(tileNumberXCoord), as.integer(tileNumberYCoord), extensionMainTile, maxView, pro)
 
  rasterizedNeighbors<-lapply(neighbors, makeRaster, Xres, Yres, pro)
  mergedNeighbors<-do.call(merge, rasterizedNeighbors)
  rm(neighbors)
  rm(rasterizedNeighbors)
  rasterizedMainTile<-makeRaster(mainTile,Xres,Yres,pro)
  rm(mainTile)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors)
  rm(mergedNeighbors)
  gc()
  
  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))
  
  r.b<-brick(rasterizedMainTile,out)
  names(r.b)<-c("Z","SVF")
  r.df<-as.data.frame(r.b,xy=TRUE)
  
  ##############################################
  ###############SAVE FUNCTION##################
  ##############################################
             
  #Writing the raster file with tile names
  writeRaster(r.b,filename=paste0(output_dir,
                                  str_pad(as.integer(floor(tileNumberXCoord/1000)*1000), 6, pad = "0"), "_",
                                  str_pad(as.integer(floor(tileNumberYCoord/1000)*1000), 6, pad = "0"),".grd"),
              format="raster",
              overwrite=TRUE)
  
  #Writing a table which will be appended if exists
  write.table(r.df,file="testSVF.txt",sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists("testSVF.txt"))
  ##############################################
  ##############################################
  ##############################################
  rm(r.svf,r.b,r.df)
  gc()
}