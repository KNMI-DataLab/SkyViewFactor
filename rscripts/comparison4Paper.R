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
library(rLiDAR)
library(microbenchmark)



#source("prepareCluster")

#master operation
outputDir<<-"/home/pagani/temp/slaves/outputNew/"
#outputDir<-"/home/ubuntu/data/slaves/outputNew/"
#outputDir2<-"/home/ubuntu/data2/outputNew/"
tempDir<<-"/home/pagani/temp/"
logDir<<-"/home/pagani/temp/slaves/log/"
dataPath<- c("/data1", "/data2", "/data3")
maxView<<-100

mergeNeighborTiles <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
  tileNeighborsLeftLowerX<-tileNumberXCoord-1000
  tileNeighborsLeftLowerY<-tileNumberYCoord-1000
  df<-loadTile22(path,tileNeighborsLeftLowerX,tileNeighborsLeftLowerY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df1<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
      df1<-checkCoordinates(df1)
    }
  }
  tileNeighborsLeftX<-tileNumberXCoord-1000
  tileNeighborsLeftY<-tileNumberYCoord
  df<-loadTile22(path,tileNeighborsLeftX,tileNeighborsLeftY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df2<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile), ymax(extensionMainTile)))
      df2<-checkCoordinates(df2)
    }
  }
  tileNeighborsLeftUpperX<-tileNumberXCoord-1000
  tileNeighborsLeftUpperY<-tileNumberYCoord+1000
  df<-loadTile22(path,tileNeighborsLeftUpperX,tileNeighborsLeftUpperY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df3<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
      df3<-checkCoordinates(df3)
    }
  }
  
  #tileNeighborsRightLower<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsRightLowerX<-tileNumberXCoord+1000
  tileNeighborsRightLowerY<-tileNumberYCoord-1000
  df<-loadTile22(path,tileNeighborsRightLowerX,tileNeighborsRightLowerY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df4<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
      df4<-checkCoordinates(df4)
    }
  }
  
  #tileNeighborRight<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz")
  tileNeighborsRightX<-tileNumberXCoord+1000
  tileNeighborsRightY<-tileNumberYCoord
  df<-loadTile22(path,tileNeighborsRightX,tileNeighborsRightY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df5<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile), ymax(extensionMainTile)))
      df5<-checkCoordinates(df5)
    }
  }
  
  
  #tileNeighborRightUpper<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsRightUpperX<-tileNumberXCoord+1000
  tileNeighborsRightUpperY<-tileNumberYCoord+1000
  df<-loadTile22(path,tileNeighborsRightUpperX,tileNeighborsRightUpperY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df6<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
      df6<-checkCoordinates(df6)
    }
  }
  
  #tileNeighborsCentralDown<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsCenterDownX<-tileNumberXCoord
  tileNeighborsCenterDownY<-tileNumberYCoord-1000
  df<-loadTile22(path,tileNeighborsCenterDownX,tileNeighborsCenterDownY)
  if(is.null(df)==FALSE){
    df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df7<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
      df7<-checkCoordinates(df7)
    }
  }
  
  #tileNeighborsCentralUp<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsCenterUpX<-tileNumberXCoord
  tileNeighborsCenterUpY<-tileNumberYCoord+1000
  df<-loadTile22(path,tileNeighborsCenterUpX,tileNeighborsCenterUpY)
  if(is.null(df)==FALSE){
     df<-makeSpatialDF(df,projection)
    extensionDF<-extent(df)
    if(xmax(extensionDF)!=xmin(extensionDF) & ymax(extensionDF)!=ymin(extensionDF)){
      df8<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
      df8<-checkCoordinates(df8)
    }
  }
  
  if(exists("df1")==FALSE){
    df1<-NULL
  }
  if(exists("df2")==FALSE){
    df2<-NULL
  }
  if(exists("df3")==FALSE){
    df3<-NULL
  }
  if(exists("df4")==FALSE){
    df4<-NULL
  }
  if(exists("df5")==FALSE){
    df5<-NULL
  }
  if(exists("df6")==FALSE){
    df6<-NULL
  }
  if(exists("df7")==FALSE){
    df7<-NULL
  }
  if(exists("df8")==FALSE){
    df8<-NULL
  }
  
  if(exists("df")==TRUE){
    rm(df)
  }
  
  dfs<-c(df1,df2,df3,df4,df5,df6,df7,df8)
  
  return(dfs)
  #########################################################
  
  
}

checkCoordinates<-function(spatialDF){
  if(is.null(spatialDF)==FALSE){
    if(xmax(spatialDF)==xmin(spatialDF) | ymax(spatialDF)==ymin(spatialDF)){
      spatialDF<-NULL
    }
  }
  spatialDF
}

checkIfMultiTile <- function(path, coordX, coordY){
  multifile<-FALSE
  file<-list.files(path = path, pattern = paste0("ahn_", coordX,"_",coordY,"_1.*"), full.names = T, recursive = T)
  if (length(file)!=0){
    multifile<-TRUE}
  rm(file)
  gc()
  return(multifile)
}  

loadTile22 <- function(path, coordX, coordY){
  coordX<-str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  if(length(files)!=0){
    currentFiles<-files#list.files(path = paste0(temp_dir, "/"), full.names = TRUE)
    message("file working on...")
    message(paste(currentFiles))
    lapply(paste(lasZipLocation, currentFiles), system)
    files_las<-gsub("laz", "las", currentFiles)
    out.matrix<-lapply(files_las, readLAS)
    outDF<-do.call(rbind.data.frame, out.matrix)
    return(outDF)
  }
  else {
    return(NULL)
  }
  
}

makeRaster<-function(spatialDF, xres, yres, pro){
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(spatialDF)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(spatialDF,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid
  return(r)
}

makeSpatialDF <- function(df,projection){
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  df
}

Xres<<-1
Yres<<-1
pro<<-CRS("+init=epsg:28992")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"
svfOldWay<-function(maxView){
Xmin=140000
#Xmax=141000
Ymin=456000
#Ymax=457000
tileNumberXCoord<-Xmin
tileNumberYCoord<-Ymin
filepath<-dataPath
mainTile<-loadTile22(filepath, Xmin, Ymin)
mainTile<-makeSpatialDF(mainTile,projection = pro)
extensionMainTile<-extent(mainTile)
if(xmax(extensionMainTile)!=xmin(extensionMainTile) & ymax(extensionMainTile)!=ymin(extensionMainTile))
{
  neighbors<-mergeNeighborTiles(dataPath, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  rasterizedNeighbors<-lapply(neighbors, makeRaster, Xres, Yres, pro)
  if(length(rasterizedNeighbors)==1){
    mergedNeighbors<-rasterizedNeighbors[[1]]
  }
  else if (length(rasterizedNeighbors)==0){
    return(-1)  
  }
  else{
    mergedNeighbors<-do.call(merge, c(rasterizedNeighbors, tolerance =10))  
  }
  rasterizedMainTile<-makeRaster(mainTile,Xres,Yres,pro)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors, tolerance = 10)
  gc()
  
  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))
  r.b<-brick(list(rasterizedMainTile,out[[1]]))
  names(r.b)<-c("Z","SVF")
  print("testtt")
  r.df<-as.data.frame(r.b,xy=TRUE)
  writeRaster(r.b,filename=paste0(outputDir,
                                  str_pad(as.integer(floor(tileNumberXCoord/1000)*1000), 6, pad = "0"), "_",
                                  str_pad(as.integer(floor(tileNumberYCoord/1000)*1000), 6, pad = "0"),".grd"),
              format="raster",
              overwrite=TRUE)

}
else {
  print(paste0("tile ", filepath, " with odd extention: ", as.character(extensionMainTile)))
}
}





computationDataCube <- function(outputDir,tempDir,logDir){
#PART FOR DATA CUBE METHOD
host<-"145.100.59.171"
reqCapabilities <- xmlParse(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCapabilities"))
xmlList<-xmlToList(reqCapabilities)
coverageId<-"HeightCoverage"
reqDescriptionCov <- xmlParse(paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=DescribeCoverage&coverageId=",coverageId))
xmlListDescriptionCov <- xmlToList(reqDescriptionCov)
lowerCorner<-strsplit(xmlListDescriptionCov[["CoverageDescription"]][["boundedBy"]][["Envelope"]][["lowerCorner"]],split = " ")
lowerCorner<-lapply(lowerCorner, as.numeric)
upperCorner<-strsplit(xmlListDescriptionCov[["CoverageDescription"]][["boundedBy"]][["Envelope"]][["upperCorner"]],split = " ")
upperCorner<-lapply(upperCorner, as.numeric)
radiusSVF<-100
Xmin=140000
Xmax=141000
Ymin=456000
Ymax=457000
message(paste(Xmin, Xmax, Ymin, Ymax))
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
          coverageExample <- paste0("http://",host,":8080/rasdaman/ows?service=WCS&version=2.0.1&request=GetCoverage&coverageId=",coverageId,"&subset=X(",xSelLow,",",xSelHigh,")&subset=Y(",ySelLow,",",ySelHigh,")&format=image/tiff")
         command<-paste0("wget -q \"", coverageExample, "\" -O ", filenameTemp)
            headerResponse <- HEAD(coverageExample)
            if(headerResponse$status_code==200){
              system(command)
            rasterFroReply = tryCatch({
              rastertest<-raster(filenameTemp)
            }, error = function(e) {
              unlink(filenameTemp)
              evalq(next)
            })
            rastertest <- reclassify(rastertest, c(-Inf, -1000,NA))
            #factor 2 to have a 1meter resolution
            rastertest<-aggregate(rastertest, fact = 2)
            message("computing SVF: ")
            rasterSVF<-svf(rastertest, nAngles = 16, maxDist = 100, ll = F)
            rasterNoFrameExt<-extent(xSelLowNoFrame,xSelHighNoFrame,ySelLowNoFrame,ySelHighNoFrame)
            tryCatch({
              rasterNoFrame<-crop(rasterSVF,rasterNoFrameExt)
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
              writeRaster(rasterNoFrame, outputFile, format="GTiff")
            }, error = function(e) {
              unlink(filenameTemp)
            })
            unlink(filenameTemp)
            }else{
              message("error getting data via WCS")
            }
            
}




#tic("taditional method")
#svfOldWay(maxView)
#toc()
# 
#tic("data cube method")
#computationDataCube(outputDir,tempDir,logDir)
#toc()


mbm <- microbenchmark("traditional" = {svfOldWay(maxView)},
                      "data_cube" = {
                        computationDataCube(outputDir,tempDir,logDir)
                      },times = 10
                        )

mbm






