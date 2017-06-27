#' Test function
#' @title Test function for a point dataset of the GMS
#' @description test function for the GMS dataset to calculate the SVF
#' @param pointX x-coordinates of the point in RD coordinates
#' @param pointY y-coordinates of the point in RD coordinates
#' @param proj projection
#' @param maxView maximum viewing distance of the svf calculation
#' @export

SVFGMSTest<-function(pointX, pointY, maxView, proj){
  
  
  #tileNumberXCoord<-str_pad(as.integer(floor(pointX/1000)*1000), 6, pad = "0")
  #tileNumberYCoord<-str_pad(as.integer(floor(pointY/1000)*1000), 6, pad = "0")
  
  tileNumberXCoord<-pointX
  tileNumberYCoord<-pointY
  
  mainTile<-loadTileGMSFromLas(lazFolder, as.integer(tileNumberXCoord), as.integer(tileNumberYCoord))
  mainTile<-makeSpatialDF(mainTile,projection = pro)
  extensionMainTile<-extent(mainTile)
  
  neighbors<-mergeNeighborTilesGSMTest(lazFolder, as.integer(tileNumberXCoord), as.integer(tileNumberYCoord), extensionMainTile, maxView, pro)
 
  rasterizedNeighbors<-lapply(neighbors, makeRaster, Xres, Yres, pro)
  mergedNeighbors<-do.call(merge, c(rasterizedNeighbors, tolerance =10))
  rm(neighbors)
  rm(rasterizedNeighbors)
  rasterizedMainTile<-makeRaster(mainTile,Xres,Yres,pro)
  rm(mainTile)
  #rasterOptions(tolerance = 0.1)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors, tolerance = 10)
  rm(mergedNeighbors)
  gc()
  
  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))
  
  #to fix the small inconsistencies in the extention (sometimes small approx are introduced in the extent)
  out<-fix_extent(rasterizedMainTile,out)

  r.b<-brick(rasterizedMainTile,out[[1]])
  names(r.b)<-c("Z","SVF")
  r.df<-as.data.frame(r.b,xy=TRUE)
  
  ##############################################
  ###############SAVE FUNCTION##################
  ##############################################
             
  #Writing the raster file with tile names
  writeRaster(r.b,filename=paste0(output_dir,
                                  str_pad(as.integer(floor(tileNumberXCoord/1000)*1000), 6, pad = "0"), "_",
                                  str_pad(as.integer(floor(tileNumberYCoord/1000)*1000), 6, pad = "0"),"-GMS.grd"),
              format="raster",
              overwrite=TRUE)
  
  #Writing a table which will be appended if exists
  write.table(r.df,file="NLSVFGMS.txt",sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists("NLSVFGMS.txt"))
  ##############################################
  ##############################################
  ##############################################
  rm(r.svf,r.b,r.df)
  gc()
}