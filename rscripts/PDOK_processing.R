#logic of the program

#list of all files available
#for each:
  #identify the 8 neighbors (if there are) by looking at the file with tile description mapping name->coordinates
  #make a full raster from the 9 tiles crop it to central tile+100m in each direction


library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(data.table)
library(stringr)
library(spatial.tools)
library(logging)
library(rjson)
library(rlist)


#####################################################################
#CLUSTER PREPARATION FUNCTION
#####################################################################
#preparing distributed cluster
prepareCluster<-function(){
  
  i<-0
  machines<-list()
  ## the users and addresses are based on the AWS configuration
  user    <- 'ubuntu'
  primary <- '172.31.15.126'
  
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
         ncore=30)
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



















#####################################################################
#DIRECTORIES NAMES GLOBAL VARS
#####################################################################
workingPath <<- getwd()

amazon<-TRUE

if(amazon){
#Amazon Dirs
dataFolder <<- "/home/ubuntu/efs/tilesPDOK/"
output_dir<<-"/home/ubuntu/efs/output/SVF_HALFMETER_NEW_HORIZON/"
logDir<<-"/home/ubuntu/efs/logPDOK/"
metadataFile<<-"/home/ubuntu/efs/tilesPDOKmapping/tilesMappingPDOKAHN2_05m.json"
#lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
#dir.create("/home/pagani/development/SkyViewFactor/data/tiles")
#temp_dir<<-"/home/pagani/development/SkyViewFactor/data/tiles/"
}else{
  output_dir<<-"/ssd1/SVFoutputPDOK05m/"
  dataFolder <<- "/ssd1/pdokDEMAHN2/"
  logDir<<-"/home/pagani/development/SkyViewFactor/log/"
  metadataFile<<-"/home/pagani/development/SkyViewFactor/inst/extdata/tilesMappingPDOKAHN2_05m.json"
}

#####################################################################



cl<-prepareCluster()




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
#INITIAL SETTINGS
#####################################################################
#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

#Xres<<-100 # x-resolution in meters
#Yres<<-100 # y-resolution in meters

radius<<-100 # max view for the SVF
angles<<-16 # number of directions



#####################################################################
#ALREADY COMPUTED FILES ANALYSIS
#####################################################################

listTiles <- list.files(path = dataFolder, ".tif", full.names = T, recursive = T)

processedFiles<-list.files(path = substr(output_dir, 1, nchar(output_dir)-1), pattern = ".tif", full.names = T)
loginfo("Checking the files already computed")

if(length(processedFiles)!=0){
  filenamesAll<-lapply(listTiles, function(x){substring(x, regexpr("[^/]+$", x))})
  filenamesWorked<-lapply(processedFiles, function(x){substring(x, regexpr("[^/]+$", x))})
  filenamesWorked<-lapply(filenamesWorked, function(x){substring(x, regexpr("[^_]+$", x))})
  alreadyWorked<-match(filenamesWorked,filenamesAll)
  tilesToStillWork<-filenamesAll[-alreadyWorked]
  tilesToStillWork<-lapply(tilesToStillWork, function(x){paste0(dataFolder,x)})
}else{
  tilesToStillWork<-listTiles
}



#####################################################################
#READING THE METADATA FILE: DESCRIPTION COORDINATE AND NAME PAIRING
#####################################################################

#getting the mapping of coordinates and filenames, required to do the merging of tiles to avoid the issue in edges
#flatten https://stackoverflow.com/questions/35444968/read-json-file-into-a-data-frame-without-nested-lists
mapping<-rjson::fromJSON(file = metadataFile)
numElements<-length(mapping$features)
tilenames_to_coords <- vector("list", numElements)
loginfo("Getting the mapping with metadata description: tiles naming - coordinates")
for(ii in 1:length(mapping$features)){
  tilenames_to_coords[[ii]]<-list(mapping$features[[ii]]$id,
                 mapping$features[[ii]]$properties$bladnr,
                 SpatialPoints(rbind(matrix(mapping$features[[ii]]$geometry$coordinates[[1]][[1]][[1]],ncol = 2),
                                     matrix(mapping$features[[ii]]$geometry$coordinates[[1]][[1]][[2]],ncol=2),
                                     matrix(mapping$features[[ii]]$geometry$coordinates[[1]][[1]][[3]],ncol=2),
                                     matrix(mapping$features[[ii]]$geometry$coordinates[[1]][[1]][[4]],ncol=2),
                                     matrix(mapping$features[[ii]]$geometry$coordinates[[1]][[1]][[5]],ncol=2))))
}


#####################################################################
#COMPUTATION
#####################################################################

loginfo("starting the parallel/distributed job")


numSlaves<-getDoParWorkers()

foreach(input=rep(paste0(logDir,"logFile.log"), numSlaves),
        .packages='logging', .export=c("loginit", "logDir")) %dopar% loginit(input)



foreach(i =  1:length(tilesToStillWork), .packages = c("raster", "horizon", "rgdal", "stringr", "logging","rlist","spatial.tools"),
        .export = c("pro", "workingPath", "radius", "tilesToStillWork","dataFolder","output_dir","logDir")) %dopar%
        {
          workerID<-paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
          loginfo(paste(workerID,"-entering foreach loop"))

          tileInWork<-tilesToStillWork[[i]]

          rastTile<-raster(tileInWork)
          print(tileInWork)
          extMainTile<-extent(rastTile)
          points<-SpatialPoints(matrix(c(xmin(extMainTile),xmax(extMainTile),xmin(extMainTile),xmax(extMainTile),ymin(extMainTile),
                                       ymin(extMainTile),ymax(extMainTile),ymax(extMainTile)),ncol = 2))
          
          
          #points<-SpatialPoints(matrix(c(210000,230000,431250,440000),ncol=2))
          tilesToMerge<-list()
          
          
          #fiinding the neighboring tiles
          
          for(i in 1:numElements){
            for(j in 1:dim(tilenames_to_coords[[i]][[3]]@coords)[1]){
            m<-(points@coords[1,] %in% tilenames_to_coords[[i]][[3]]@coords[j,])
            n<-(points@coords[2,] %in% tilenames_to_coords[[i]][[3]]@coords[j,])
            k<-(points@coords[3,] %in% tilenames_to_coords[[i]][[3]]@coords[j,])
            w<-(points@coords[4,] %in% tilenames_to_coords[[i]][[3]]@coords[j,])
            if ((m[1]==TRUE & m[2]==TRUE) | (n[1]==TRUE & n[2]==TRUE) | (k[1]==TRUE & k[2]==TRUE) | (w[1]==TRUE & w[2]==TRUE)){
              tilesToMerge<-list.append(tilesToMerge,paste0(dataFolder,"r",tilenames_to_coords[[i]][2],".tif"))
            }
            }
          }
          
          tilesToMergeUnique<-unique(as.array(tilesToMerge))
          
          tilesRaster<-lapply(tilesToMergeUnique, raster)
          mergedRaster<-do.call(merge, tilesRaster)
          
          croppingExtension<-extMainTile+200
          
          
          rasterForHorizon<-crop(mergedRaster,croppingExtension)
          
          ##JUST FOR TEST TO BE REMOVED####
          #raster50m<-aggregate(rasterForHorizon,fact=100)
          
          
          svfTile<-svf(rasterForHorizon,nAngles = angles, maxDist = radius, ll=F)
          
          out<-crop(svfTile,extMainTile)
          
          
          #r.b<-brick(rasterizedMainTile,out[[1]])
          #names(r.b)<-c("Z","SVF")
          #r.df<-as.data.frame(r.b,xy=TRUE)
          
          ##############################################
          ###############SAVE FUNCTION##################
          ##############################################
          filenameMainTile<-substring(tileInWork, regexpr("[^/]+$", tileInWork))
          
          #Writing the raster file with tile names
          writeRaster(out,filename=paste0(output_dir, "SVF_",filenameMainTile),
                      format="GTiff",
                      overwrite=TRUE)
          loginfo(paste(workerID,"witten file",paste0(output_dir, "SVF_",filenameMainTile)))
          
}
