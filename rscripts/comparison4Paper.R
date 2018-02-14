library(XML)
library(foreach)
library(doParallel)
library(httr)
library(tiff)
library(raster)
library(rgdal)
library(rtiff)
library(horizon)
library(uuid)
library(logging)
library(stringr)
library(tictoc)








#TO BE LOOKED AGAIN INTO

loadTile22 <- function(path, coordX, coordY){
  
  coordX<-str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  
  #uuid<-UUIDgenerate()
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  dir.create(paste0(temp_dir,uuid,"/"))
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  if(length(files)!=0){
    #lapply(files,file.copy,to=paste0(temp_dir,uuid,"/"))
    currentFiles<-files#list.files(path = paste0(temp_dir, "/"), full.names = TRUE)
    print(paste(currentFiles))
    lapply(paste(lasZipLocation, currentFiles), system)
    #system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
    #system(paste0("rm ", temp_dir, uuid,"/*.laz"))
    files_las<-list.files(paste0(temp_dir, uuid),pattern="*.las$",full.names = TRUE)
    out.matrix<-lapply(files_las, readLAS)
    outDF<-do.call(rbind.data.frame, out.matrix)
    #out<-data.frame(out.matrix)
    #system(paste0("rm ", temp_dir, uuid,"/*.las"))
    
    #rm(out.matrix,multifiles,uuid,centralFile,files,currentFiles,files_las)
    #gc()
    return(outDF)
  }
  else {
    return(NULL)
  }
  
}




















#source("prepareCluster")

#master operation
outputDir<-"/home/pagani/temp/slaves/outputNew/"
#outputDir<-"/home/ubuntu/data/slaves/outputNew/"
#outputDir2<-"/home/ubuntu/data2/outputNew/"

tempDir<-"/home/pagani/temp/"

logDir<-"/home/pagani/temp/slaves/log/"



computationDataCube <- function(outputDir,tempDir,logDir){



#PART FOR DATA CUBE METHOD
#logDir<-"/home/ubuntu/data/slaves/log/"
host<-"145.100.59.171"
#host<-"10.100.253.1"
reqCapabilities <- xmlParse(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCapabilities"))
xmlList<-xmlToList(reqCapabilities)
#coverageId<-xmlList[["Contents"]][["CoverageSummary"]][["CoverageId"]]
coverageId<-"HeightCoverage"

reqDescriptionCov <- xmlParse(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=DescribeCoverage&coverageId=",coverageId))
xmlListDescriptionCov <- xmlToList(reqDescriptionCov)
lowerCorner<-strsplit(xmlListDescriptionCov[["CoverageDescription"]][["boundedBy"]][["Envelope"]][["lowerCorner"]],split = " ")
lowerCorner<-lapply(lowerCorner, as.numeric)

upperCorner<-strsplit(xmlListDescriptionCov[["CoverageDescription"]][["boundedBy"]][["Envelope"]][["upperCorner"]],split = " ")
upperCorner<-lapply(upperCorner, as.numeric)

# yDistance<-abs(upperCorner[[1]][[2]]-lowerCorner[[1]][[2]])
# xDistance<-abs(upperCorner[[1]][[1]]-lowerCorner[[1]][[1]])
# 
# Xmax<-max(upperCorner[[1]][[1]],lowerCorner[[1]][[1]])
# Xmin<-min(upperCorner[[1]][[1]],lowerCorner[[1]][[1]])
# 
# Ymax<-max(upperCorner[[1]][[2]],lowerCorner[[1]][[2]])
# Ymin<-min(upperCorner[[1]][[2]],lowerCorner[[1]][[2]])

#processingTileSideX<-xDistance/100

radiusSVF<-100

Xmin=140000
Xmax=141000
Ymin=456000
Ymax=457000

message(paste(Xmin, Xmax, Ymin, Ymax))

#slaveBand<-yDistance/numSlaves

#processingTileSideY<-slaveBand/40



#logger setup
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file=paste0(logDir,"testing.log"), level='DEBUG')
with(getLogger(), names(handlers))



#logger for slaves

loginit <- function(logfile) {
  library(logging)
  basicConfig(level='FINEST')
  addHandler(writeToFile, file=paste0(logDir,"testing.log"), level='DEBUG')
  with(getLogger(), names(handlers))
  NULL
}




Xmin=140000
Xmax=141000
Ymin=456000
Ymax=457000


          
          
          xSelLowNoFrame <- Xmin
          xSelHighNoFrame <- Xmax
          ySelLowNoFrame <- Ymin
          ySelHighNoFrame <- Ymax
          
          xSelLow <- xSelLowNoFrame-radiusSVF
          xSelHigh <- xSelHighNoFrame+radiusSVF
          ySelLow <- ySelLowNoFrame-radiusSVF
          ySelHigh <- ySelHighNoFrame+radiusSVF
          
          
          
          
          xSelLowCh<-as.character(round(xSelLow))
          xSelHighCh<-as.character(round(xSelHigh))
          ySelLowCh<-as.character(round(ySelLow))
          ySelHighCh<-as.character(round(ySelHigh))
          
          filenameTemp<-paste0(tempDir,xSelLowCh,"_",xSelHighCh,"--",ySelLowCh,"_",ySelHighCh,"-","temp.tiff")
          
          
          xSelLowNoFrCh<-as.character(round(xSelLowNoFrame))
          xSelHighNoFrCh<-as.character(round(xSelHighNoFrame))
          ySelLowNoFrCh<-as.character(round(ySelLowNoFrame))
          ySelHighNoFrCh<-as.character(round(ySelHighNoFrame))
          
          
            
            #loginfo(paste(workerID,"--examining region ", xSelLow, " ", xSelHigh, " ", ySelLow, " ", ySelHigh))
            
            coverageExample <- paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(",xSelLow,",",xSelHigh,")&subset=Y(",ySelLow,",",ySelHigh,")&format=image/tiff")
            
            
            
            
            
            
            
            
            command<-paste0("wget -q \"", coverageExample, "\" -O ", filenameTemp)
            headerResponse <- HEAD(coverageExample)
            if(headerResponse$status_code==200){
              tic("gettingCoverage")
              system(command)

            
            
            
          
            rasterFroReply = tryCatch({
              rastertest<-raster(filenameTemp)
              #loginfo(paste(workerID,"--FINISHED processing raster: ", filenameTemp))
            }, error = function(e) {
              #logerror(paste0(workerID,"--error writing file ",filenameTemp, " error ", e$message, " coverage: ", coverageExample))
              unlink(filenameTemp)
              evalq(next)
            })
            
            
            
            
            rastertest <- reclassify(rastertest, c(-Inf, -1000,NA))
            
            #factor 2 to have a 1meter resolution
            rastertest<-aggregate(rastertest, fact = 10)

            
            message("computing SVF: ")
            #tic("SVFcomputation")
            rasterSVF<-svf(rastertest, nAngles = 16, maxDist = 100, ll = F)
            rasterNoFrameExt<-extent(xSelLowNoFrame,xSelHighNoFrame,ySelLowNoFrame,ySelHighNoFrame)
            #timer<-toc()
            #loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
            tryCatch({
              #tic("cropRaster")
              rasterNoFrame<-crop(rasterSVF,rasterNoFrameExt)
              #timer<-toc()
              #loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
              
            },
            
            error = function(e) {
              logerror(paste0(workerID,"--error in cropping extent raster with frame: ",as.character(extent(rasterSVF))[[1]], " ",
                              as.character(extent(rasterSVF))[[2]], " ",
                              as.character(extent(rasterSVF))[[3]], " ",
                              as.character(extent(rasterSVF))[[4]], " ",
                              "extent raster to crop to: ",
                              as.character(extent(rasterNoFrameExt))[[1]], " ",
                              as.character(extent(rasterNoFrameExt))[[2]], " ",
                              as.character(extent(rasterNoFrameExt))[[3]]," ",
                              as.character(extent(rasterNoFrameExt))[[4]]," ",
                              " error ", e$message, " BBox called was ", coverageExample))
            })
            
            outputFile<-paste0(outputDir,"cubeFileOutput.tif")
            result = tryCatch({
              #tic("writeResultSVFtiff")
              writeRaster(rasterNoFrame, outputFile, format="GTiff")
              #loginfo(paste(workerID,"--written SVF raster at file ", outputFile))
              #timer<-toc()
              #loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
            }, error = function(e) {
              #logerror(paste0(workerID,"--error writing file ",outputFile, " error ", e$message))
              unlink(filenameTemp)
              #evalq(next)
            })
            
            
            
            
            unlink(filenameTemp)
            }else{
              message("error getting data via WCS")
            }
            
}

