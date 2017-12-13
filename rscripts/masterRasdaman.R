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



#master operation
outputDir<-"/home/ubuntu/data/slaves/output/"
logDir<-"/home/ubuntu/data/slaves/log/"
host<-"10.100.253.2"
reqCapabilities <- xmlParse(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCapabilities"))
xmlList<-xmlToList(reqCapabilities)
coverageId<-xmlList[["Contents"]][["CoverageSummary"]][["CoverageId"]]

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

processingTileSideX<-xDistance/100

radiusSVF<-100

numSlaves<-3

slaveBand<-yDistance/numSlaves

processingTileSideY<-slaveBand/40
  


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






#coverageExample <- GET(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(140000,141000)&subset=Y(456200,458200)&format=image/tiff"))


#contentTIFF<-content(coverageExample,"raw")
#cl <- makeCluster(numSlaves)
cl<-prepareCluster()
registerDoParallel(cl)
loginfo("cluster created")


foreach(input=rep('~/Desktop/out.log', numSlaves), 
      .packages='logging') %dopar% loginit(input)


foreach(i =  1:numSlaves, .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid", "logging", "httr"), 
  .export = c( "radiusSVF")) %dopar%{
    #i=1   
    
    #for(i in 1:numSlaves){
                #print("ABC")
                Xmin=140000
                Ymin=306250
                
                
                xInitial = Xmin
                
                #ySlaveBand<-i * slaveBand
                yInitial = Ymin + slaveBand*(i-1)
                loginfo(paste0(i," ", i, " ", yInitial))
                
                xP<-xInitial
                yP<-yInitial
                
                
              
                
                numTilesX<-xDistance/processingTileSideX 
                numTilesY<-slaveBand/processingTileSideY
                
                for(yside in 1:numTilesY){
                  if (yP==Ymin){
                    ySelLow = Ymin
                    ySelHigh = ySelLow + processingTileSideY + radiusSVF
                    yLowNoFrame = ySelLow
                    yHighNoFrame = ySelHigh - radiusSVF
                  } else{
                    ySelLow = yInitial + processingTileSideY*(yside-1) -radiusSVF
                    ySelHigh = ySelLow + processingTileSideY + 2*radiusSVF
                    yLowNoFrame = ySelLow + radiusSVF
                    yHighNoFrame = ySelHigh - radiusSVF
                  }
                  if(ySelHigh>Ymax){
                    ySelHigh = Ymax
                    yHighNoFrame = ySelHigh
                  }
                  loginfo(paste0(i," yLow: ", ySelLow, " yHigh: ", ySelHigh," yinit:",yInitial))
                  for(xside in 1:2){#numTilesX){
                    if(xP==Xmin){
                      xSelLow = Xmin
                      xSelHigh = xSelLow + processingTileSideX + radiusSVF
                      xLowNoFrame = xSelLow
                      xHighNoFrame = xSelHigh - radiusSVF
                    } else{
                      xSelLow = xInitial + processingTileSideX*(xside-1) -radiusSVF
                      xSelHigh = xSelLow + processingTileSideX + 2*radiusSVF
                      xLowNoFrame = xSelLow + radiusSVF
                      xHighNoFrame = xSelHigh - radiusSVF
                    }
                    
                    if(xSelHigh>Xmax){
                      xSelHigh = Xmax
                      xHighNoFrame = xSelHigh
                    }
                    #print(c(i, xP, yP))
                    coverageExample <- paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(",xSelLow,",",xSelHigh,")&subset=Y(",ySelLow,",",ySelHigh,")&format=image/tiff")

                    xSelLowCh<-as.character(round(xSelLow))
                    xSelHighCh<-as.character(round(xSelHigh))
                    ySelLowCh<-as.character(round(ySelLow))
                    ySelHighCh<-as.character(round(ySelHigh))
                    
                    filenameTemp<-paste0(outputDir,xSelLowCh,"_",xSelHighCh,"--",ySelLowCh,"_",ySelHighCh,"-","temp.tiff")
                    
                    
                    xSelLowNoFrCh<-as.character(round(xLowNoFrame))
                    xSelHighNoFrCh<-as.character(round(xHighNoFrame))
                    ySelLowNoFrCh<-as.character(round(yLowNoFrame))
                    ySelHighNoFrCh<-as.character(round(yHighNoFrame))
                    
                    outputFile<-paste0(outputDir,xSelLowNoFrCh,"_",xSelHighNoFrCh,"--",ySelLowNoFrCh,"_",ySelHighNoFrCh,".tiff")
                    
                    
                    
                    
                    loginfo(paste0("getting coverage: ", coverageExample, " for file ", filenameTemp))

                    uuidVal<-UUIDgenerate()

                    command<-paste0("wget -q \"", coverageExample, "\" -O ", filenameTemp)


                    headerResponse <- HEAD(coverageExample)

                    if(headerResponse$status_code==200){
                    system(command)
                    } else{
                      logerror(paste0("response status server ",headerResponse$status_code, " coverage ",
                                     coverageExample, " not available"))
                      next
                    }

                    
                    #print(coverageExample)
                    xP =  xInitial + processingTileSideX*(xside)
                    #tiffRaw<-coverageExample$content
                    #tiffByte<-readTIFF(tiffRaw)
                    #writeTiff(tiffByte,"temp.tiff")
                    
                    
                    loginfo(paste0("processing raster: ", filenameTemp))
                    
                    rasterFroReply = tryCatch({
                      rastertest<-raster(filenameTemp)
                      loginfo(paste0("FINISHED processing raster: ", filenameTemp))
                    }, error = function(e) {
                      logerror(paste0("error writing file ",filenameTemp, " error ", e$message, " coverage: ", coverageExample))
                    })
                    
                    
                    
                    
                    
                   

                    rastertest <- reclassify(rastertest, c(-Inf, -1000,NA))
                    rastertest<-aggregate(rastertest, fact = 15)
                    message("computing SVF: ")
                    rasterSVF<-svf(rastertest, nAngles = 16, maxDist = 100, ll = F)
                    rasterNoFrameExt<-extent(xLowNoFrame,xHighNoFrame,yLowNoFrame,yHighNoFrame)
                    tryCatch({
                      rasterNoFrame<-crop(rasterSVF,rasterNoFrameExt)},
                      error = function(e) {
                        logerror(paste0("error in cropping extent raster with frame: ",as.character(extent(rasterSVF))[[1]], " ",
                                        as.character(extent(rasterSVF))[[2]], " ",
                                        as.character(extent(rasterSVF))[[3]], " ",
                                        as.character(extent(rasterSVF))[[4]], " ",
                                        "extent raster to crop to: ", 
                                        as.character(extent(rasterNoFrameExt))[[1]], " ", 
                                        as.character(extent(rasterNoFrameExt))[[2]], " ",
                                        as.character(extent(rasterNoFrameExt))[[3]]," ",
                                        as.character(extent(rasterNoFrameExt))[[4]]," ",
                                        " error ", e$message))
                    })
                    
                    #outputFile<-paste0(outputDir,uuidVal,"--SVF.tiff")
                    
                    result = tryCatch({
                      writeRaster(rasterNoFrame, outputFile, format="GTiff")
                      loginfo(paste0("written SVF raster at file ", outputFile))
                    }, error = function(e) {
                      logerror(paste0("error writing file ",outputFile, " error ", e$message))
                    })
                    
                    
                    
                    
                    #loginfo(paste0("written SVF raster at file ", outputFile))
                    #plot(rastertest)
                    #plot(rasterSVF)
                    unlink(filenameTemp)
                    
                  }
                  xP = xInitial
                  yP =  yInitial + processingTileSideY*(yside)
                }
            }

