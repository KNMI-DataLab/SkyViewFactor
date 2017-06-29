#' Point information from the grid files
#' @title Point info from grid
#' @description returns the grid file name the point is in
#' @param point a point with x-coords and y-coords in RD
#' @param n layer number of the grid
#' 
#' @author Marieke Dirksen
#' @export
#@param save_dir save file name with extention .txt

point_info_from_grid<-function(point=df[1,],n=2){
  #save_dir="data/test_point.txt"
  point$tileNumberXCoord<-floor(point$lon/1000)*1000
  point$tileNumberYCoord<-floor(point$lat/1000)*1000
  tiles_unique<-unique(point[c("tileNumberXCoord","tileNumberYCoord")])
  tiles_unique_names<-paste0(str_pad(as.integer(tiles_unique$tileNumberXCoord), width = 6, pad = "0"),"_",
                             str_pad(as.integer(tiles_unique$tileNumberYCoord),  width = 6, pad = "0"), ".grd")
  # GMS_meta<-as(GMS_meta,"SpatialPoints")
  
  #Grid files with SVF
  grids.location<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"
  #grid.files.loc<-list.files(grids.location,pattern=".grd",full.names=TRUE)
  grid.files.name<-list.files(grids.location,pattern=".grd",full.names=FALSE)
  
  I<-which(tiles_unique_names %in% grid.files.name)
  
  ###########################################
  grid.file<-paste0(grids.location,tiles_unique_names[I])
  point<-point[I,]
  point$grid.file<-grid.file
  
  
  # grid<-as.character(point["grid.file"])
  # br<-brick(grid)
  # r<-br[[n]]
  # 
  # sgdf<-as(r,"SpatialGridDataFrame")
  # 
  # coordinates(point)<-~lon+lat
  # crs(point)<-crs(sgdf)
  # 
  # svf.point<-over(point,sgdf)
  # 
  # svf.extract<-raster::extract(x=r,y=point,buffer=10,fun=mean,na.rm=TRUE)
  # svf.extract.sd<-raster::extract(x=r,y=point,buffer=10,fun=sd,na.rm=TRUE)
  # 
  # df.svf<-cbind(point,svf.point)
  
  #sampled.svf = apply(X = points, MARGIN = 1, FUN = function(points) r@data@values[which.min(replace(distanceFromPoints(r, points), is.na(r), NA))])
  #https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel
  
  out<-data.frame(point)
  names(out)<-c("DS_CODE","lon","lat","optional","tileNumberXCoord", "tileNumberYCoord" ,"grid.file")
  
  #out<-data.frame(point,df.svf[2],svf.extract,svf.extract.sd)
  #names(out)<-c("DS_CODE","lon","lat","optional","tileNumberXCoord", "tileNumberYCoord" ,"grid.file","optional","svf.over","svf.extract","svf.extract.sd")
  #write.table(out,file=save_dir,sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists(save_dir))
  
  return(out)
}
































