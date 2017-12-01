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

#All the functions are stored in the folder functions
#With the library R.utils and the command sourceDirectory all the functions are loaded
library(R.utils)
sourceDirectory("functions/")

#####################################################################
#DIRECTORIES
#####################################################################
workingPath <<- getwd()

#Andrea
#output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF_200m/"
#dataFolder <<- "/efs/tiles"
dataFolder <<- "/data1/lidarTilesGTiff_1m"
output_dir<<-"/home/pagani/temp/efs/output/"
logDir<<-"/home/pagani/temp/efs/log/"
#lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
#temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"

#Marieke
# output_dir<<-"/home/dirksen/SVF/gridsSVF/"
# lazFolder <<- c("/data1/", "/data2/", "/data3")
# lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
# dir.create("/home/dirksen/SVF/temp/")
# temp_dir<<-"/home/dirksen/SVF/temp/"

#####################################################################

#####################################################################
#INITIAL SETTINGS
#####################################################################
#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

Xres<<-100 # x-resolution in meters
Yres<<-100 # y-resolution in meters

maxView<<-100 # max view for the SVF

registerDoParallel(10) #number of parallel cores
#####################################################################



prepareCLuster<-function(){
  
  
  #cl<-makePSOCKcluster(2)
  #primary<-'127.0.0.1'
  #user<-'andrea'
  #machineAddresses <- list(
  #    list(host=primary,user=user,
  #      ncore=2)
  # )
  
  ## make cluster
  #registerDoParallel(cores=3)
  
  i<-0
  machines<-list()
  ## the users and addresses are based on the AWS configuration
  user    <- 'ubuntu'
  primary <- '172.31.39.160'
  
  #IPs contains a list of slaves that will run the computations
  IPs<-paste0("172.31.46.", seq(from = 157, to = 174))
  #IPs<-c(IPs, "172.31.38.73") ##slave gold master machine
  IPs<-c("172.31.38.73")
  for (ip in IPs){
    i<-i+1
    machines[[i]]<-list(host=ip, user = user, ncore=16)
  }
  
  machineAddresses <- list(
    list(host=primary,user=user,
         ncore=1)
  )
  machineAddresses<-machines #c(machineAddresses,machines)
  
  #characteristics of the cluster are assigned (e.g., IPs, hosts, users, IPs)
  spec <- lapply(machineAddresses,
                 function(machine) {
                   rep(list(list(host=machine$host,
                                 user=machine$user)),
                       machine$ncore)
                 })
  spec <- unlist(spec,recursive=FALSE)
  
  #cluster is created (the communication between master and slaves takes place on the port 11000 and is a SSH-like session)
  parallelCluster <- parallel::makeCluster(type='PSOCK',
                                           master=primary,
                                           spec=spec,
                                           port=11000, outfile="")
  print(parallelCluster)
  
  
  #source("./R/CoreFeatureCompute.R")# we might export those functions
  
  
  ##some libraries and functions are explicitly exported
  #clusterEvalQ(parallelCluster, library(imager), FileNameParser())
  parallelCluster <- cl
  registerDoParallel(parallelCluster)
  
  
  
}









main<-function(){

listTiles <- list.files(path = dataFolder, ".tif", full.names = T, recursive = T)

#listTiles <- listTiles[40001:length(listTiles)]


#SVF(tiles_unique[1,]$tileNumberXCoord, tiles_unique[1,]$tileNumberYCoord,maxView, pro)

logfile<-paste0(logDir,"logNewAWS.txt")
logfile2<-paste0(logDir,"22logNewAWS.txt")


system.time(
foreach(i =  1:length(listTiles), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "maxView", "Xres", "Yres",
                    "loadNeighborTile_v2","mergeNeighborTiles"), .combine = f(length(listTiles))) %dopar%
{
  write(paste("processing ", i, listTiles[[i]]),file = logfile, append = T)
  outp<-1
  tilesToBeWorked<-getTileNumber(listTiles[[i]])
  #write(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri"), file = logfile2, append = T)
    if(!file.exists(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri")))
     {

      
      #print("ABC")
    # print(paste0(output_dir,
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
    #              str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri"))
      tryCatch(outp<-SVFWholeNL(listTiles[[i]],maxView), error=function(e){write(paste0(e, "tile ", listTiles[[i]]," with error"),file = logfile, append = T); return(NULL)})

      #SVFWholeNL(listTiles[[i]],maxView)

      #tryCatch(outp<-SVF(coord[[i]][1], coord[[i]][2],maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
      if(is.null(outp))
      {
        print(paste("error in tile ",listTiles[[i]]))
        next
      }
      else if (outp == -1)
      {
        write(paste("tile has no neighbors",listTiles[[i]]),logfile, append = T)
        
      }
    gc()
    }
  else {
   # print(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri", " already computed"))
  }
})

#unlink(temp_dir, recursive = T)
}


getTileNumber <- function(filepath){
  
  
  file<-basename(filepath)
  splits<-unlist(strsplit(file, c("\\.")))
  splits<-unlist(strsplit(splits[[1]], "_"))
  
  
  coordX<-splits[[2]] #str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-splits[[3]]
  
  coordsXY<-c(str_pad(as.integer(coordX), width = 6, pad = "0"), str_pad(as.integer(coordY), width = 6, pad = "0"))
  rm(file,splits,coordX,coordY)
  return(coordsXY)
  
}





#Progress combine function
f <- function(n){
  pb <- txtProgressBar(min=1, max=n-1,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    #Sys.sleep(5)
    flush.console()
    c(...)
  }
}



