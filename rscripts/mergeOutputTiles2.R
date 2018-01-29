library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(uuid)
library(data.table)
library(stringr)
library(spatial.tools)
library(parallel)
library(SpaDES)
library(ncdf4)

output_dir<<-"/home/ubuntu/efs/output/SVF_1m/"
splits_dir<<-"/home/ubuntu/efs/output/SVF_1m_regionsNew/"


files<-list.files(output_dir,full.names = T, pattern = "*.gri")


rasterOutput<-function(x){
  ras<-brick(x)
  rr<-ras[[2]]
  rr
}



mergeAndSplit<-function(files){
rasterOptions(maxmemory=58e9)
message("building cluster")
cl<-makeCluster(12, type = "FORK")


message("select the layer and getting the raster from every tile")
wholeRasterList<-parLapply(cl,files,rasterOutput)
stopCluster(cl)

#rasterOptions(tolerance = 100)
#options(overlap=F)

message("starting merging the tiles")
totalRaster<-do.call(merge, c(wholeRasterList, list(tolerance=100)))
message("full raster merged, now split in 16 regions")
#totalRaster<-raster("/home/ubuntu/efs/output/tempRastersCopy/r_tmp_2018-01-10_213252_2546_27069.grd")
sections<-splitRaster(totalRaster,4,4,path =splits_dir)

}


removeArtifacts<-function(rr){
#to check if there are artifacts where SVF<0
#to be corrected with
s<-calc(rr, fun=function(x){x[x<0]<-0; return(x)})
s
}

convertToNetCDF<-function(file){
  ras<-raster(file)
  ras<-removeArtifacts(ras)
  writeRaster(ras,gsub(".gri",".nc",file))
}

changesToNetCDFFileForKDC<-function(netCDFFile){
  
  ncFile<-nc_open(netCDFFile)
  x<-ncvar_get(ncFile,'easting')
  y<-ncvar_get(ncFile,'northing')
  coordsProjString<-ncatt_get(ncFile, 0, attname ='crs')
  svf<-ncvar_get(ncFile,"layer")
  
  
  
  xD <- ncdim_def( "x", "m", x)
  yD <- ncdim_def( "y", "m", y)
  #xVar<-ncvar_def("x","projection_x_coordinate",dim = xD)
  #yVar<-ncvar_def("y","projection_y_coordinate",dim = yD)
  svfVar<-ncvar_def("svf","",list(xD,yD), missval = -3.4e+38, longname = "sky_view_factor", prec = 'float')
  coordVar<-ncvar_def("projection","",dim = list(), prec = 'char')
  prodVar<-ncvar_def("product", "", dim = list(), prec="char")
  
  newNC<-nc_create(gsub(".nc","_kdcVers.nc",netCDFFile),list(svfVar,coordVar,prodVar))
  
  ncvar_put(newNC,xVar,x)
  ncvar_put(newNC,yVar,y)
  ncvar_put(newNC,svfVar,svf)
  
  today <- Sys.time()
  now<-format(today, format="%Y-%m-%dT%H:%M:%S")
  
  ncatt_put(newNC, varid = "x", attname = "long_name", attval = "x coordinate of projection")
  ncatt_put(newNC, varid = "y", attname = "long_name", attval = "y coordinate of projection")
  ncatt_put(newNC, varid = svfVar, attname = "grid_mapping", attval = "projection")
  ncatt_put(newNC, varid = "x", attname = "standard_name", attval = "projection_x_coordinate")
  ncatt_put(newNC, varid = "y", attname = "standard_name", attval = "projection_y_coordinate")
  ncatt_put(newNC, varid = prodVar, attname = "date_start_of_data", attval = "20070101T000000")
  ncatt_put(newNC, varid = prodVar, attname = "date_end_of_data", attval = "20130101T000000")
  ncatt_put(newNC, varid = prodVar, attname = "creation_date", attval = now)
  ncatt_put(newNC, varid = prodVar, attname = "originator", attval = "Royal Netherlands Meteorological Institute (KNMI)")
  ncatt_put(newNC, varid = coordVar, attname = 'proj4_params', attval = coordsProjString$value, prec = "char")
  ncatt_put(newNC, varid = 0, attname = "Conventions", attval = "CF-1.4")
  ncatt_put(newNC, varid = 0, attname = "comment", attval = "Sky view factor based on the AHN2 height dataset, grid at 1 meter resolution. Sky view factor computation using R package horizon (v 1.0) with parameters: 16 directions, 100m radius.")
  ncatt_put(newNC, varid = 0, attname = "title", attval = "Sky view factor")
  ncatt_put(newNC, varid = 0, attname = "source", attval = "Actueel Hoogbestand Nederland (http://www.ahn.nl/)")
  ncatt_put(newNC, varid = 0, attname = "history", attval = "E-science center and Wageningen University post-processing on source point cloud data")
  ncatt_put(newNC, varid = 0, attname = "institution", attval = "Royal Netherlands Meteorological Institute (KNMI)")
  
  
  
  
  nc_close(newNC)
 
  

}



#main
rasterOptions(maxmemory=20e9)
cl<-makeCluster(16, type = "FORK")
parLapply(cl, files,convertToNetCDF)
filesNC<-list.files(output_dir,full.names = T, pattern = "*.nc")
parLapply(cl,filesNC,changesToNetCDFFileForKDC)
stopCluster(cl)



