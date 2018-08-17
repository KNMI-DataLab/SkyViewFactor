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
library(logging)

#All the functions are stored in the folder functions
#With the library R.utils and the command sourceDirectory all the functions are loaded
library(R.utils)
sourceDirectory("functions/")




#####################################################################
#DIRECTORIES NAMES GLOBAL VARS
#####################################################################
workingPath <<- getwd()

#Amazon Dirs
#output_dir<<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF_200m/"
dataFolder <<- "/home/ubuntu/efs/tiles/"
#dataFolder <<- "/data1/lidarTilesGTiff_1m"
output_dir<<-"/home/ubuntu/efs/output/SVF_1m_NEW_HORIZON/"
logDir<<-"/home/ubuntu/efs/log/"
#lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
#temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"


#####################################################################


#####################################################################
#LOGGER SETUP
#####################################################################
#logger setup
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=paste0(logDir,"logFile.log"), level='DEBUG')
with(getLogger(), names(handlers))


#logger for slaves

loginit <- function(logfile) {
  library(logging)
  basicConfig(level='FINEST')
  addHandler(writeToFile, file=paste0(logDir,"logFile.log"), level='DEBUG')
  with(getLogger(), names(handlers))
  NULL
}

#####################################################################


#####################################################################
#INITIAL SETTINGS
#####################################################################
#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

#Xres<<-100 # x-resolution in meters
#Yres<<-100 # y-resolution in meters

maxView<<-100 # max view for the SVF

#registerDoParallel(2) #number of parallel cores
#####################################################################



#####################################################################
#CLUSTER PREPARATION FUNCTION
#####################################################################
#preparing distributed cluster
prepareCluster<-function(){
  
  i<-0
  machines<-list()
  ## the users and addresses are based on the AWS configuration
  user    <- 'ubuntu'
  primary <- '172.31.39.160'
  
  #IPs contains a list of slaves that will run the computations
  #IPs<-paste0("172.31.422.", seq(from = 157, to = 174))
  IPs<-c("172.31.5.221", "172.31.1.23")
 ##slave gold master machine
  #IPs<-c("172.31.38.73")
  for (ip in IPs){
    i<-i+1
    machines[[i]]<-list(host=ip, user = user, ncore=48)
  }
  
  machineAddresses <- list(
    list(host=primary,user=user,
         ncore=28)
  )
  machineAddresses<-c(machineAddresses,machines)
  
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
  message("Creating cluster")
  print(parallelCluster)
  
  
  #source("./R/CoreFeatureCompute.R")# we might export those functions
  
  
  ##some libraries and functions are explicitly exported
  #clusterEvalQ(parallelCluster, library(imager), FileNameParser())
  #parallelCluster <- cl
  registerDoParallel(parallelCluster)
  message("Cluster created")
  loginfo("Cluster created")
  
 return(parallelCluster)
}

#####################################################################







main<-function(){
  
  numSlaves<-getDoParWorkers()
  
  foreach(input=rep(paste0(logDir,"logFile.log"), numSlaves),
          .packages='logging') %dopar% loginit(input)
  
  
  
  
  

listTiles <- list.files(path = dataFolder, ".grd", full.names = T, recursive = T)

processedFiles<-list.files(path = substr(output_dir, 1, nchar(output_dir)-1), pattern = ".grd", full.names = T)


#listTiles <- listTiles[40001:length(listTiles)]


#SVF(tiles_unique[1,]$tileNumberXCoord, tiles_unique[1,]$tileNumberYCoord,maxView, pro)


foreach(i =  1:length(listTiles), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid","stringr", "logging"),
        .export = c("getTileNumber","loadTile", "checkIfMultiTile", "makeSpatialDF", "makeRaster", "pro", "workingPath", "maxView", "mergeNeighborTiles", "listTiles","dataFolder","output_dir","logDir","SVFWholeNL","loadTileWholeNL","checkCoordinates","fix_extent")) %dopar%
{
  workerID<-paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
  
  outp<-1
  tilesToBeWorked<-getTileNumber(listTiles[[i]])
  loginfo(paste(workerID,"--examining region ",tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]]))
  #write(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri"), file = logfile2, append = T)
    if(file.exists(paste0(output_dir, tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]], ".gri"))==FALSE)
     {

      
      loginfo(paste(workerID,"--processing tile ",tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]]))
      tryCatch(outp<-SVFWholeNL(listTiles[[i]],maxView), error=function(e){
        loginfo(paste0(e, "tile ", listTiles[[i]]," with error")); return(NULL)})
      #print(outp)
      #SVFWholeNL(listTiles[[i]],maxView)

      #tryCatch(outp<-SVF(coord[[i]][1], coord[[i]][2],maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
      if(is.null(outp))
      {
        message(paste("error in tile ",listTiles[[i]]))
        next
      }
      else if (outp == -1)
      {
        message(paste("tile has no neighbors",listTiles[[i]]))
        loginfo(paste("tile has no neighbors",listTiles[[i]]))
      }
    gc()
    }
  else {
   loginfo(paste(workerID,"--examined region ",tilesToBeWorked[[1]],"_", tilesToBeWorked[[2]]), "ALREADY COMPUTED")
  }
}

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
# f <- function(n){
#   pb <- txtProgressBar(min=1, max=10,style=3) #temporaly converted to from n-1 to 10
#   count <- 0
#   function(...) {
#     count <<- count + length(list(...)) - 1
#     setTxtProgressBar(pb,count)
#     #Sys.sleep(5)
#     flush.console()
#     c(...)
#   }
# }
# 


