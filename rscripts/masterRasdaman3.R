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



#source("prepareCluster")

#master operation
#outputDir<-"/home/pagani/temp/slaves/outputNew/"
outputDir<-"/home/ubuntu/data/slaves/outputNew/"
outputDir2<-"/home/ubuntu/data2/outputNew/"

tempDir<-"/home/ubuntu/temp/"

#logDir<-"/home/pagani/temp/slaves/log/"
logDir<-"/home/ubuntu/data/slaves/log/"
#host<-"145.100.59.171"
host<-"10.100.253.1"
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

yDistance<-abs(upperCorner[[1]][[2]]-lowerCorner[[1]][[2]])
xDistance<-abs(upperCorner[[1]][[1]]-lowerCorner[[1]][[1]])

Xmax<-max(upperCorner[[1]][[1]],lowerCorner[[1]][[1]])
Xmin<-min(upperCorner[[1]][[1]],lowerCorner[[1]][[1]])

Ymax<-max(upperCorner[[1]][[2]],lowerCorner[[1]][[2]])
Ymin<-min(upperCorner[[1]][[2]],lowerCorner[[1]][[2]])

#processingTileSideX<-xDistance/100

radiusSVF<-100

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


#localSlaves<-20

#cl <- makeCluster(localSlaves)
cl<-prepareCluster()
registerDoParallel(cl)
loginfo("cluster created")
numSlaves<-getDoParWorkers()


foreach(input=rep('~/Desktop/out.log', numSlaves),
      .packages='logging') %dopar% loginit(input)






#xcoord<-c(Xmin,Xmax)
#ycoord<-c(Ymin,Ymax)


xCoordsCells<-seq(from=Xmin, to=Xmax, by = 2000)
yCoordsCells<-seq(from=Ymin, to=Ymax, by = 2000)

fullCoords<-list()

counter<-1

for(i in 1:(length(xCoordsCells)-1)){
  for(j in 1:(length(yCoordsCells)-1)){
    if(i==1){
    xcellMin<-xCoordsCells[[i]]
    }
    else{
      xcellMin<-xCoordsCells[[i]]-radiusSVF
    }
    if(i==length(xCoordsCells)-1){
    xcellMax<-xCoordsCells[[i+1]]
    }
    else{
      xcellMax<-xCoordsCells[[i+1]]+radiusSVF
    }
    if(j==1){
      ycellMin<-yCoordsCells[[j]]
    }
    else{
      ycellMin<-yCoordsCells[[j]]-radiusSVF
    }
    if(j==length(yCoordsCells)-1){
      ycellMax<-yCoordsCells[[j+1]]
    }
    else{
      ycellMax<-yCoordsCells[[j+1]]+radiusSVF
    }
    #first 4 coords are the one to be used for the query, other 4 are to be used for crop for remove radius frame
    coordsForQuery<-c(xcellMin,xcellMax,ycellMin,ycellMax,xCoordsCells[[i]],xCoordsCells[[i+1]],yCoordsCells[[j]],yCoordsCells[[j+1]])
    #coordsFor
    #print(coordsForQuery)
    fullCoords[[counter]]<-coordsForQuery
    counter<-counter+1
  }
}



#tic("processing files")
processedFiles1<-list.files(path = substr(outputDir, 1, nchar(outputDir)-1), pattern = ".tif", full.names = T)
processedFiles2<-list.files(path = substr(outputDir2, 1, nchar(outputDir2)-1), pattern = ".tif", full.names = T)
processedFiles<-c(processedFiles1,processedFiles2)

#timer<-toc()
#loginfo(round(timer$toc-timer$tic,digits = 3))



foreach(i =  1:length(fullCoords), .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid", "logging", "httr","stringr","tictoc"),
  .export = c( "radiusSVF", "processedFiles", "fullCoords")) %dopar%{
   # i=1

   # for(i in 1:length(fullCoords)){
                #print("ABC")
                #Xmin=140000
                #Ymin=306250
                workerID<-paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')


                print(fullCoords[[i]])
                
                xSelLow <- fullCoords[[i]][[1]]
                xSelHigh <- fullCoords[[i]][[2]]
                ySelLow <- fullCoords[[i]][[3]]
                ySelHigh <- fullCoords[[i]][[4]]
                
                xSelLowNoFrame <- fullCoords[[i]][[5]]
                xSelHighNoFrame <- fullCoords[[i]][[6]]
                ySelLowNoFrame <- fullCoords[[i]][[7]]
                ySelHighNoFrame <- fullCoords[[i]][[8]]
                



                xSelLowCh<-as.character(round(xSelLow))
                xSelHighCh<-as.character(round(xSelHigh))
                ySelLowCh<-as.character(round(ySelLow))
                ySelHighCh<-as.character(round(ySelHigh))

                filenameTemp<-paste0(tempDir,xSelLowCh,"_",xSelHighCh,"--",ySelLowCh,"_",ySelHighCh,"-","temp.tiff")


                xSelLowNoFrCh<-as.character(round(xSelLowNoFrame))
                xSelHighNoFrCh<-as.character(round(xSelHighNoFrame))
                ySelLowNoFrCh<-as.character(round(ySelLowNoFrame))
                ySelHighNoFrCh<-as.character(round(ySelHighNoFrame))

                outputFile<-paste0(outputDir2,xSelLowNoFrCh,"_",xSelHighNoFrCh,"--",ySelLowNoFrCh,"_",ySelHighNoFrCh,".tif")
                outputFileOldDir<-paste0(outputDir,xSelLowNoFrCh,"_",xSelHighNoFrCh,"--",ySelLowNoFrCh,"_",ySelHighNoFrCh,".tif")
                



                if(sum(str_detect(processedFiles,outputFile))==0 & sum(str_detect(processedFiles,outputFileOldDir))==0){


                loginfo(paste(workerID,"--examining region ", xSelLow, " ", xSelHigh, " ", ySelLow, " ", ySelHigh))

               coverageExample <- paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(",xSelLow,",",xSelHigh,")&subset=Y(",ySelLow,",",ySelHigh,")&format=image/tiff")






                    loginfo(paste(workerID,"--getting coverage: ", coverageExample, " for file ", filenameTemp))

                    uuidVal<-UUIDgenerate()

                    #guarantee a random choice properly
                    set.seed(i)
                    Sys.sleep(round(runif(1, min=0, max=numSlaves)))
                    
                    
                    tic("headerCoverage")
                    command<-paste0("wget -q \"", coverageExample, "\" -O ", filenameTemp)
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))




                    retry<-0
                    for(retry in 1:5){
                    headerResponse <- HEAD(coverageExample)
                    if(headerResponse$status_code==200){
                    tic("gettingCoverage")
                    system(command)
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
                    break
                    } else if(headerResponse$status_code==500){
                      logerror(paste0(workerID,"--response status server ",headerResponse$status_code, " coverage ",
                                     coverageExample, " not available"))
                      Sys.sleep(30)
                    }
                    else{
                      logerror(paste0(workerID,"--response status server ",headerResponse$status_code, " coverage ",
                                      coverageExample, " not available"))
                    }
                      
                    }
                    if(retry==5){
                      logerror(paste0(workerID,"--FATAL ERROR: RETRIES ARE OVER--response status server ",headerResponse$status_code, " coverage ",
                                      coverageExample, " not available"))
                      evalq(next)
                    }


                    #print(coverageExample)
                    #xP =  xInitial + processingTileSideX*(xside)
                    #tiffRaw<-coverageExample$content
                    #tiffByte<-readTIFF(tiffRaw)
                    #writeTiff(tiffByte,"temp.tiff")


                    loginfo(paste(workerID,"--processing raster: ", filenameTemp))
                    
                    tic("rasterFromTiff")
                    rasterFroReply = tryCatch({
                      rastertest<-raster(filenameTemp)
                      loginfo(paste(workerID,"--FINISHED processing raster: ", filenameTemp))
                    }, error = function(e) {
                      logerror(paste0(workerID,"--error writing file ",filenameTemp, " error ", e$message, " coverage: ", coverageExample))
		                  unlink(filenameTemp)
		                  evalq(next)
                    })
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))






                    tic("removing NAs")
                    rastertest <- reclassify(rastertest, c(-Inf, -1000,NA))
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
                    
                    ###############REMOVE IN THE FINAL COMPUTATION####################
                   # rastertest<-aggregate(rastertest, fact = 20)
                    ##################################################################
                    
                    
                    message("computing SVF: ")
                    tic("SVFcomputation")
                    rasterSVF<-svf(rastertest, nAngles = 16, maxDist = 100, ll = F)
                    rasterNoFrameExt<-extent(xSelLowNoFrame,xSelHighNoFrame,ySelLowNoFrame,ySelHighNoFrame)
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
                    tryCatch({
                      tic("cropRaster")
                      rasterNoFrame<-crop(rasterSVF,rasterNoFrameExt)
                      timer<-toc()
                      loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
                      
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

                    #outputFile<-paste0(outputDir,uuidVal,"--SVF.tiff")

                    result = tryCatch({
                      tic("writeResultSVFtiff")
                      writeRaster(rasterNoFrame, outputFile, format="GTiff")
                      loginfo(paste(workerID,"--written SVF raster at file ", outputFile))
                      timer<-toc()
                      loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))
                    }, error = function(e) {
                      logerror(paste0(workerID,"--error writing file ",outputFile, " error ", e$message))
		                  unlink(filenameTemp)
		                  evalq(next)
                    })




                    #loginfo(paste0("written SVF raster at file ", outputFile))
                    #plot(rastertest)
                    #plot(rasterSVF)
                    tic("removingTempFile")
                    unlink(filenameTemp)
                    #remove the temporary raster files created
                    removeTmpFiles(h=8)
                    timer<-toc()
                    loginfo(paste(workerID,"--", timer$msg, round(timer$toc-timer$tic,digits = 3)))

                }else{
                  
                  if(sum(str_detect(processedFiles,outputFile))==0){
                    fileWrittenLocation<-outputFile
                  }else{
                    fileWrittenLocation<-outputFileOldDir
                  } 
                  
                  loginfo(paste(workerID,"--file ", fileWrittenLocation, " already processed, skipping it"))
                  message(paste0("file ", fileWrittenLocation, " already processed, skipping it"))
                }
  }
                  #xP = xInitial
                 # yP =  yInitial + processingTileSideY*(yside)



