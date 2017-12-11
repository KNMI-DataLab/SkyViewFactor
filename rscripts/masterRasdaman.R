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
outputDir<-"/home/pagani/temp/"
host<-"145.100.59.171"
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

numSlaves<-15

slaveBand<-yDistance/numSlaves

processingTileSideY<-slaveBand/20
  


#logger setup
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file="~/testing.log", level='DEBUG')
with(getLogger(), names(handlers))



#logger for slaves

loginit <- function(logfile) {
  library(logging)
  basicConfig(level='FINEST')
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  with(getLogger(), names(handlers))
  NULL
}






#coverageExample <- GET(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(140000,141000)&subset=Y(456200,458200)&format=image/tiff"))


#contentTIFF<-content(coverageExample,"raw")
cl <- makeCluster(numSlaves)
registerDoParallel(cl)
loginfo("cluster created")


foreach(input=rep('~/Desktop/out.log', numSlaves), 
      .packages='logging') %dopar% loginit(input)


foreach(i =  1:numSlaves, .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid", "logging", "httr"), 
  .export = c( "radiusSVF")) %dopar%{
    #i=1            
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

                    loginfo(paste0("getting coverage: ", coverageExample))

                    uuidVal<-UUIDgenerate()

                    command<-paste0("wget -q \"", coverageExample, "\" -O ", outputDir,uuidVal,"temp.tiff")


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
                    
                    filename<-paste0(outputDir,uuidVal,"temp.tiff")
                    loginfo(paste0("processing raster: ", filename))
                    rastertest<-raster(filename)

                    rastertest <- reclassify(rastertest, c(-Inf, -1000,NA))
                    rastertest<-aggregate(rastertest, fact = 20)
                    message("computing SVF: ")
                    rasterSVF<-svf(rastertest, nAngles = 16, maxDist = 100, ll = F)
                    rasterNoFrameExt<-extent(xLowNoFrame,xHighNoFrame,yLowNoFrame,yHighNoFrame)
                    rasterNoFrame<-crop(rasterSVF,rasterNoFrameExt)
                    outputFile<-paste0(outputDir,uuidVal,"--SVF.tiff")
                    
                    result = tryCatch({
                      writeRaster(rasterNoFrame, outputFile, format="GTiff")
                      loginfo(paste0("written SVF raster at file ", outputFile))
                    }, error = function(e) {
                      logerror(paste0("error writing file ",outputFile, " error ", e$message))
                    })
                    
                    
                    
                    
                    #loginfo(paste0("written SVF raster at file ", outputFile))
                    #plot(rastertest)
                    #plot(rasterSVF)
                    unlink(filename)
                    
                  }
                  xP = xInitial
                  yP =  yInitial + processingTileSideY*(yside)
                }
            }

