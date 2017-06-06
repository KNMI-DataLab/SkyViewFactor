library(raster)
pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")
#GMS data
file.points<-fread("allstations_edit.txt")
names(file.points)<-c("name","lat","lon","netwerk")

coordinates(file.points)<-~lon+lat
crs(file.points)<-WGS84
file.points<-spTransform(file.points,CRSobj=pro)

df<-data.frame(file.points)

df$tileNumberXCoord<-floor(df$lon/1000)*1000
df$tileNumberYCoord<-floor(df$lat/1000)*1000
tiles_unique<-unique(df[c("tileNumberXCoord","tileNumberYCoord")])
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
save_dir<-"data/SVF_Nadir.txt"
df<-df[I,]
df$grid.file<-grid.file

for (i in 1:length(df$name)){

points<-df[i,]
grid<-grid.file[i]
br<-brick(grid)
r<-br[[2]]

r<-as(r,"SpatialGridDataFrame")

coordinates(points)<-~lon+lat
crs(points)<-crs(r)
svf.point<-over(points,r)

df.svf<-cbind(points,svf.point)
#sampled.svf = apply(X = points, MARGIN = 1, FUN = function(points) r@data@values[which.min(replace(distanceFromPoints(r, points), is.na(r), NA))])
#https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel

#out<-cbind(df.svf,sampled.svf)
out<-data.frame(points,df.svf[2])
write.table(out,file=save_dir,sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists(save_dir))
}















#To DO: create a new dataframe with points and grid file names in one, so lapply function does work!
lapply(df,extract_point_from_grid,
       save_dir=save_dir)


#############################################
extract_point_from_grid<-function(df,save_dir){
#require a point and a grid file for the same location  
  grid<-df$grid.file
  br<-brick(grid)
  r<-br[[2]]
  
  r<-as(r,"SpatialGridDataFrame")
  
  svf.point<-over(GMS_meta,r)
  
  df.svf<-cbind(points,svf.point)
  if(!is.na(df.svf$SVF)){
    message("point has NA value in raster")
  }
  
  sampled.svf = apply(X = points, MARGIN = 1, FUN = function(points) r@data@values[which.min(replace(distanceFromPoints(r, points), is.na(r), NA))])
  #https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel
  
  out<-cbind(df.svf,sampled.svf)
  
  write.table(out,file=save_dir,sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists(save_dir))
  return(TRUE)
}