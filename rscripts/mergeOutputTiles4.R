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
library(gdalUtils)

library(logging)


tiles_dir<<-"/home/ubuntu/efs/output/SVF_1m_NEW_HORIZON/"
splits_dir<<-"/home/ubuntu/efs/output/SVF_1m_NEW_HORIZON_regions/"

logDir<<-"/home/ubuntu/efs/log/"
#logDir<-"/home/pagani/temp/efs/log/"
#output_dir<<-"/home/pagani/temp/efs/output/"

files<-list.files(tiles_dir,full.names = T, pattern = "*.gri")




#logger setup
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=paste0(logDir,"logFileMerge.log"), level='DEBUG')
with(getLogger(), names(handlers))


#logger for slaves

loginit <- function(logfile) {
  library(logging)
  basicConfig(level='FINEST')
  addHandler(writeToFile, file=paste0(logDir,"logFileMerge.log"), level='DEBUG')
  with(getLogger(), names(handlers))
  NULL
}








rasterOutput<-function(x){
  ras<-brick(x)
  rr<-ras[[2]]
  workerID<-paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
  loginfo(paste0(workerID,"--SVF extraction for tile",as.character(extent(rr))))
  loginfo("--------------------------")
  rr
}



mergeAndSplit<-function(files){
#rasterOptions(maxmemory=58e9)
message("building cluster")
cl<-makeCluster(62, type = "FORK")


numSlaves<-getDoParWorkers()

foreach(input=rep(paste0(logDir,"logFile.log"), numSlaves),
        .packages='logging', .export=c("loginit", "logDir")) %dopar% loginit(input)



loginfo("select the layer and getting the raster from every tile")

message("select the layer and getting the raster from every tile")
wholeRasterList<-parLapply(cl,files,rasterOutput)
stopCluster(cl)

#rasterOptions(tolerance = 100)
#options(overlap=F)

message("starting merging the tiles")

#e <- extent(-131, -124, 49, 53)
#template <- raster(e)
#projection(template) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
#writeRaster(template, file="fullRaster.tif", format="GTiff")
#mosaic_rasters(gdalfile = files,dst_datase="/home/ubuntu/efs/output/SVF_1m_NEW_HORIZON_regions/fullRaster.tif",of="GTiff")
#do.call(merge, c(wholeRasterList, list(tolerance=100)))
fullRaster<-do.call(mosaic,c(wholeRasterList, list(fun=mean,tolerance=100)))
loginfo("full raster merged")
message("full raster merged")

#message("full raster merged, now split in 16 regions")
#totalRaster<-raster("/home/ubuntu/efs/output/tempRastersCopy/r_tmp_2018-01-10_213252_2546_27069.grd")
sections<-splitRaster(totalRaster,4,4,path =splits_dir)
loginfo("sections done")
message("sections done")
}


removeArtifacts<-function(rr){
#to check if there are artifacts where SVF<0
#to be corrected with
s<-calc(rr, fun=function(x){x[x<0]<-0; return(x)})
s
}


checkInconsistent<-function(fileName){
  ras<-brick(fileName)
  rr<-ras[[2]]
  rr
  #to check if there are artifacts where SVF<0
  #to be corrected with
  #s<-calc(rr, fun=function(x){if(x<0){loginfo(paste("tile with issues",extent(rr)))}})
  if(min(values(rr),na.rm = T)<0)
  {loginfo(paste0("tile with issues",as.character(extent(rr))))
    loginfo("--------------------------")}
  #s
}





convertToNetCDF<-function(file){
message("converting raster to NetCDF")
  ras<-raster(file)
message("raster read, removing artifacts")
  ras<-removeArtifacts(ras)
message("artifacts removed, writing raster")
  writeRaster(ras,gsub(".gri",".nc",file))
}



convertToGeoTiff<-function(file){
  message("converting raster to GeoTiff")
  ras<-raster(file)
  message("raster read, removing artifacts")
  ras<-removeArtifacts(ras)
  crs(ras)<-CRS("+init=epsg:28992")
  message("artifacts removed, writing raster")
  writeRaster(ras,gsub(".gri",".tif",file), format="GTiff")
}



changesToNetCDFFileForKDC<-function(netCDFFile){
  
  ncFile<-nc_open(netCDFFile)
  x<-ncvar_get(ncFile,'easting')
  y<-ncvar_get(ncFile,'northing')
  coordsProjString<-ncatt_get(ncFile, 0, attname ='crs')
  svf<-ncvar_get(ncFile,"layer")
  
  
  
  xD <- ncdim_def( "x", "m", x)
  yD <- ncdim_def( "y", "m", y)
  xVar<-ncvar_def("x","projection_x_coordinate",dim = xD)
  yVar<-ncvar_def("y","projection_y_coordinate",dim = yD)
  svfVar<-ncvar_def("svf","",list(xD,yD), missval = -3.4e+38, longname = "sky_view_factor", prec = 'float')
  coordVar<-ncvar_def("projection","",dim = list(), prec = 'char')
  prodVar<-ncvar_def("product", "", dim = list(), prec="char")
  
  newNC<-nc_create(gsub(".nc","_kdcVers.nc",netCDFFile),list(svfVar,coordVar,prodVar), force_v4=TRUE, verbose=TRUE)
  
  ncvar_put(newNC,xVar,x)
  ncvar_put(newNC,yVar,y)
  legX<-floor(dim(svf)[1]/5)
  firstLegXStop<-legX
  secondLegXStart<-firstLegXStop+1
  secondLegXStop<-secondLegXStart+legX
  thirdLegXStart<-secondLegXStop+1
  thirdLegXStop<-thirdLegXStart+legX
  
  fourthLegXStart<-thirdLegXStop+1
  fourthLegXStop<-fourthLegXStart+legX
  fifthLegXStart<-fourthLegXStop+1
  fifthLegXStop<-dim(svf)[1]
  

  message(dim(svf)[2])
#  firstLegy<-floor(dim(svf)[2]/2)
 # secondLegyStart<-firstLegy+1 
  secondLegyStop<-dim(svf)[2]
  message(length(svf))
  ncvar_put(newNC,svfVar,svf[1:firstLegXStop,1:secondLegyStop],start=c(1,1), count=c(firstLegXStop,secondLegyStop))
  ncvar_put(newNC,svfVar,svf[secondLegXStart:secondLegXStop,1:secondLegyStop],start=c(secondLegXStart,1), count=c(secondLegXStop-secondLegXStart+1,secondLegyStop))
  
  ncvar_put(newNC,svfVar,svf[thirdLegXStart:thirdLegXStop,1:secondLegyStop],start=c(thirdLegXStart,1), count=c(thirdLegXStop-thirdLegXStart+1,secondLegyStop))
  ncvar_put(newNC,svfVar,svf[fourthLegXStart:fourthLegXStop,1:secondLegyStop],start=c(fourthLegXStart,1), count=c(fourthLegXStop-fourthLegXStart+1,secondLegyStop))
  
  ncvar_put(newNC,svfVar,svf[fifthLegXStart:fifthLegXStop,1:secondLegyStop],start=c(fifthLegXStart,1), count=c(fifthLegXStop-fifthLegXStart+1,secondLegyStop))
today <- Sys.time()
  now<-format(today, format="%Y-%m-%dT%H:%M:%S")
  
  ncatt_put(newNC, varid = "x", attname = "long_name", attval = "x_coordinate_of_projection")
  ncatt_put(newNC, varid = "y", attname = "long_name", attval = "y_coordinate_of_projection")
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


mergeAndSplit(files)




#rasterOptions(maxmemory=250e9)
#cl<-makeCluster(2, type = "FORK", outfile="")
#parLapply(cl, files,convertToGeoTiff)
#message("conversion to GeoTiff finished")
#filesNC<-list.files(output_dir,full.names = T, pattern = "*.nc")
#message("starting insertion of metadata")
#changesToNetCDFFileForKDC(filesNC[1])
#lapply(filesNC,changesToNetCDFFileForKDC)
#message("files NetCDF created")
#stopCluster(cl)



