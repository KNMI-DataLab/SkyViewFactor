library(XML)
library(foreach)
library(doParallel)
library(httr)
library(tiff)


#master operation
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

numSlaves<-10

slaveBand<-yDistance/numSlaves

processingTileSideY<-slaveBand/10
  


#coverageExample <- GET(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(140000,141000)&subset=Y(456200,458200)&format=image/tiff"))


#contentTIFF<-content(coverageExample,"raw")


foreach(i =  1:numSlaves, .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"),
  .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
              "pro", "workingPath", "lazFolder", "lasZipLocation", "maxView", "Xres", "Yres",
              "loadNeighborTile_v2","mergeNeighborTiles"), .combine = f(length(listTiles))) %do%{
                xInitial = Xmin
                
                ySlaveBand<-i * slaveBand
                yInitial = Ymin + ySlaveBand*(i-1)
                
                xP<-xInitial
                yP<-yInitial
              
                
                numTilesX<-xDistance/processingTileSideX 
                numTilesY<-slaveBand/processingTileSideY
                
                for(yside in 1:numTilesY){
                  if (yP==Ymin){
                    ySelLow = Ymin
                    ySelHigh = ySelLow + processingTileSideY + radiusSVF
                  } else{
                    ySelLow = yInitial + processingTileSideY*(yside-1) -radiusSVF
                    ySelHigh = ySelLow + processingTileSideY + 2*radiusSVF
                  }
                  if(ySelHigh>Ymax){
                    ySelHigh = Ymax
                  }
                  for(xside in 1:2){#numTilesX){
                    if(xP==Xmin){
                      xSelLow = Xmin
                      xSelHigh = xSelLow + processingTileSideX + radiusSVF
                    } else{
                      xSelLow = xInitial + processingTileSideX*(xside-1) -radiusSVF
                      xSelHigh = xSelLow + processingTileSideX + 2*radiusSVF
                    }
                    
                    if(xSelHigh>Xmax){
                      xSelHigh = Xmax
                    }
                    print(c(xP, yP))
                    coverageExample <- GET(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(",xSelLow,",",xSelHigh,")&subset=Y(",ySelLow,",",ySelHigh,")&format=image/tiff"))
                    print(coverageExample)
                    xP =  xInitial + processingTileSideX*(xside)
                    
                    
                  }
                  yP =  yInitial + processingTileSideY*(yside)
                }
              }

