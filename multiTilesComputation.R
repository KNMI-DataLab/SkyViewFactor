library(raster)
library(horizon)
library(rgdal)
library(rLiDAR)
library(foreach)
library(doParallel)
library(uuid)
library(data.table)
library(stringr)


#global vars/ config vars
pro<<-CRS("+init=epsg:28992")
WGS84<<-CRS("+init=epsg:4326")

lazFolder <<- c("/data1/", "/data2/", "/data3")
lasZipLocation <<- "/home/pagani/tools/LAStools/bin/laszip"

Xres<<-5 # x-resolution in meters
Yres<<-5 # y-resolution in meters

maxView<<-100





GMS_meta<-fread("GMS stations metadata (incl. regio en coordinator) 2016_2017 v20161010.csv")
coordinates(GMS_meta)<-~loc_lon+loc_lat
crs(GMS_meta)<-WGS84
GMS_meta<-spTransform(GMS_meta,CRSobj=pro)

registerDoParallel(8)

workingPath <<- getwd()



main<-function(){

pointX<- 237000 #244001
pointY<- 500000#576001

c1<-c(pointX, pointY)
coordsGMS<-as(GMS_meta,"SpatialPoints")
coordsGMS<-data.frame(coordsGMS)

coordsGMS$tileNumberXCoord<-floor(coordsGMS$loc_lon/1000)*1000
coordsGMS$tileNumberYCoord<-floor(coordsGMS$loc_lat/1000)*1000

#tiles_unique<-unique(coordsGMS[c("tileNumberXCoord","tileNumberYCoord")])

 pointX2<- 198000
 pointY2<- 500000
 
 pointX3<-175000
 pointY3<-558000
   
   pointX4<-054000
   pointY4<-407000
   
   pointX5<-084000
   pointY5<-445000
   
   pointX6<-189000
   
   pointY6<-352000
   
   pointX7<-034000
   pointY7<-389000
   pointX8<-165000
   pointY8<-475000
 
 
# 
 c2<-c(pointX2, pointY2)
 c3<-c(pointX3, pointY3)
 
 c4<-c(pointX4, pointY4)
 c5<-c(pointX5, pointY5)
 c6<-c(pointX6, pointY6)
 c7<-c(pointX7, pointY7)
 c8<-c(pointX8, pointY8)
 
 
# 
coord <<- list(c1,  c2,c3,c4,c5,c6,c7,c8)

dir.create("/home/pagani/development/SkyViewFactor/data/tiles")

#1:length(coordsGMS[,1])

system.time(
foreach(i = 1:length(coordsGMS[,1]) , .packages = c("raster", "horizon", "rgdal", "rLiDAR", "uuid"), 
        .export = c("loadTile", "checkMultiTile", "makeSpatialDF", "loadNeighborTiles","makeRaster",
                    "pro", "workingPath", "lazFolder", "lasZipLocation", "maxView", "Xres", "Yres", "coord")) %dopar%
{
  print(i)
  #print(paste0(workingPath,"/data/gridsSVF/",
             # str_pad(floor(coordsGMS[i,]$loc_lon/1000)*1000, 6, pad = "0"),"_", 
             #  str_pad(floor(coordsGMS[i,]$loc_lat/1000)*1000,  6, pad = "0"),".gri"))
  #print(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000))
  #print(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000))
    # 
  
  outp<-1
    if(!file.exists(paste0(workingPath,"/data/gridsSVF/",
                            str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
                            str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri")))
       {
       #print("ABC")
     print(paste0(workingPath,"/data/gridsSVF/",
                  str_pad(as.integer(floor(coordsGMS[i,]$loc_lon/1000)*1000), 6, pad = "0"),"_",
                  str_pad(as.integer(floor(coordsGMS[i,]$loc_lat/1000)*1000),  6, pad = "0"), ".gri"))
      tryCatch(SVF(coordsGMS[i,]$loc_lon, coordsGMS[i,]$loc_lat,maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
  
  #tryCatch(outp<-SVF(coord[[i]][1], coord[[i]][2],maxView, pro), error=function(e){print(paste0("tile with point x=", coord[[i]][1], " y=",coord[[i]][2],"not available in dataset. Skipping point.")); return(NULL)})
  if(is.null(outp))
  {
    next
  }
  gc()
  #}
  #SVF(coord[i,]$loc_lon, coord[i,]$loc_lat,maxView, pro)
    }
}

)
#26 rasters were computed without error, checking the 27th and 28th file
#SVF(133743.9, 445509.3,maxView, pro)

unlink("/home/pagani/development/SkyViewFactor/data/tiles/", recursive = T)

}




##############################


#allTiles<-list.files(path = lazFolder, "*.laz", full.names = T, recursive = T)

loadTile <- function(path, coordX, coordY){
  
  coordX<-str_pad(as.integer(floor(coordX/1000)*1000), 6, pad = "0")
  coordY<-str_pad(as.integer(floor(coordY/1000)*1000), 6, pad = "0")
  
  uuid<-UUIDgenerate()
  multifileFlag<-checkIfMultiTile(path, coordX, coordY)
  multifiles <- NULL
  if(multifileFlag){
    multifiles<-list.files(path = path, pattern = paste0("ahn_", coordX,"_", coordY,"_.*"), full.names = T, recursive = T)
  }
  dir.create(paste0(workingPath,"/data/tiles/",uuid,"/"))
  centralFile<-list.files(path = path, paste0("ahn_", coordX,"_", coordY,".laz"), full.names = T, recursive = T)
  files<-c(centralFile,multifiles)
  if(length(files)!=0){
  lapply(files,file.copy,to=paste0(workingPath,"/data/tiles/",uuid,"/"))
  currentFiles<-list.files(path = paste0(workingPath,"/data/tiles/", uuid,"/"), full.names = TRUE)
  lapply(paste(lasZipLocation, currentFiles), system)
  #system("/usr/people/pagani/opt/testFile/LAStools/bin/laszip .laz")
  system(paste0("rm ", workingPath, "/data/tiles/", uuid,"/*.laz"))
  files_las<-list.files(paste0(workingPath, "/data/tiles/", uuid),pattern="*.las$",full.names = TRUE)
  out.matrix<-lapply(files_las, readLAS)
  outDF<-do.call(rbind.data.frame, out.matrix)
  #out<-data.frame(out.matrix)
  system(paste0("rm ", workingPath, "/data/tiles/", uuid,"/*.las"))
  rm(out.matrix)
  outDF
  }
  else NULL
}



checkIfMultiTile <- function(path, coordX, coordY){
  multifile<-FALSE
  file<-list.files(path = path, pattern = paste0("ahn_", coordX,"_",coordY,"_1.*"), full.names = T, recursive = T)
  if (length(file)!=0){
    multifile<-TRUE}
  multifile 
}  

##when the input is  a tile and not a coord pair
# checkIfMultiTileInputTile <- function(lazfilepath){
#   multifile<-FALSE
#   pathSplit<-strsplit(lazfilepath,split = "ahn",)[[1]]
#   filename<-paste0("ahn",pathSplit[[length(filename)]])
#   folder<-strsplit(pathSplit[[1]])
#   rootFilename <- strsplit(filename, split = ".")[[1]]
#   rootFilename<-rootFilename[[1]]
#   
#   
#   file<-list.files(path = folder, pattern = paste0(rootFilename,"_1.*"), full.names = T, recursive = T)
#   if (length(file)!=0){
#     multifile<-TRUE}
#   multifile 
# }  

makeSpatialDF <- function(df,projection){
  coordinates(df)<-~X+Y
  proj4string(df)<-pro
  df
}

loadNeighborTiles <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
  #print("hello")
  #tileNeighborsLeftLower<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsLeftLowerX<-tileNumberXCoord-1000
  tileNeighborsLeftLowerY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsLeftLowerX,tileNeighborsLeftLowerY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df1<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  }

  #tileNeighborLeft<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord,".laz")
  tileNeighborsLeftX<-tileNumberXCoord-1000
  tileNeighborsLeftY<-tileNumberYCoord
  df<-loadTile(path,tileNeighborsLeftX,tileNeighborsLeftY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df2<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile), ymax(extensionMainTile)))
  }
  
  #tileNeighborLeftUpper<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsLeftUpperX<-tileNumberXCoord-1000
  tileNeighborsLeftUpperY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsLeftUpperX,tileNeighborsLeftUpperY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df3<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  }

  #tileNeighborsRightLower<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsRightLowerX<-tileNumberXCoord+1000
  tileNeighborsRightLowerY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsRightLowerX,tileNeighborsRightLowerY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df4<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  }

  #tileNeighborRight<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz")
  tileNeighborsRightX<-tileNumberXCoord+1000
  tileNeighborsRightY<-tileNumberYCoord
  df<-loadTile(path,tileNeighborsRightX,tileNeighborsRightY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df5<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile), ymax(extensionMainTile)))
  }

  #tileNeighborRightUpper<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsRightUpperX<-tileNumberXCoord+1000
  tileNeighborsRightUpperY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsRightUpperX,tileNeighborsRightUpperY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df6<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
  }

  #tileNeighborsCentralDown<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz")
  tileNeighborsCenterDownX<-tileNumberXCoord
  tileNeighborsCenterDownY<-tileNumberYCoord-1000
  df<-loadTile(path,tileNeighborsCenterDownX,tileNeighborsCenterDownY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df7<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
  }

  #tileNeighborsCentralUp<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz")
  tileNeighborsCenterUpX<-tileNumberXCoord
  tileNeighborsCenterUpY<-tileNumberYCoord+1000
  df<-loadTile(path,tileNeighborsCenterUpX,tileNeighborsCenterUpY)
  if(is.null(df)==FALSE){
  df<-makeSpatialDF(df,projection)
  df8<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
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
    df1<-NULL
  }
  
  if(exists("df")==TRUE){
    rm(df)
  }
  
  gc()
  dfs<-c(df1,df2,df3,df4,df5,df6,df7,df8)
}



# loadNeighborTilesTest <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
#   #print("hello")
#   #tileNeighborsLeftLower<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord-1000,".laz")
#   tileNeighborsLeftLowerX<-tileNumberXCoord-1000
#   tileNeighborsLeftLowerY<-tileNumberYCoord-1000
#   df<-loadTile(path,tileNeighborsLeftLowerX,tileNeighborsLeftLowerY)
#   df<-makeSpatialDF(df,projection)
#   df1<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
# 
#   #tileNeighborLeft<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord,".laz")
#   #tileNeighborsLeftX<-tileNumberXCoord-1000
#   #tileNeighborsLeftY<-tileNumberYCoord
#   #df<-loadTile(path,tileNeighborsLeftX,tileNeighborsLeftY)
#   #df<-makeSpatialDF(df,projection)
#   #df2<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymin(extensionMainTile), ymax(extensionMainTile)))
# 
#   # #tileNeighborLeftUpper<-paste0("ahn_", tileNumberXCoord-1000,"_",tileNumberYCoord+1000,".laz")
#   # tileNeighborsLeftUpperX<-tileNumberXCoord-1000
#   # tileNeighborsLeftUpperY<-tileNumberYCoord+1000
#   # df<-loadTile(path,tileNeighborsLeftUpperX,tileNeighborsLeftUpperY)
#   # df<-makeSpatialDF(df,projection)
#   # df3<-crop(df,c(xmin(extensionMainTile)-maxView,xmin(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
#   #
#   # #tileNeighborsRightLower<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord-1000,".laz")
#   # tileNeighborsRightLowerX<-tileNumberXCoord+1000
#   # tileNeighborsRightLowerY<-tileNumberYCoord-1000
#   # df<-loadTile(path,tileNeighborsRightLowerX,tileNeighborsRightLowerY)
#   # df<-makeSpatialDF(df,projection)
#   # df4<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
#   #
#   # #tileNeighborRight<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord,".laz")
#   # tileNeighborsRightX<-tileNumberXCoord+1000
#   # tileNeighborsRightY<-tileNumberYCoord
#   # df<-loadTile(path,tileNeighborsRightX,tileNeighborsRightY)
#   # df<-makeSpatialDF(df,projection)
#   # df5<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymin(extensionMainTile), ymax(extensionMainTile)))
#   #
#   # #tileNeighborRightUpper<-paste0("ahn_", tileNumberXCoord+1000,"_",tileNumberYCoord+1000,".laz")
#   # tileNeighborsRightUpperX<-tileNumberXCoord+1000
#   # tileNeighborsRightUpperY<-tileNumberYCoord+1000
#   # df<-loadTile(path,tileNeighborsRightUpperX,tileNeighborsRightUpperY)
#   # df<-makeSpatialDF(df,projection)
#   # df6<-crop(df,c(xmax(extensionMainTile),xmax(extensionMainTile)+maxView,ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
#   #
#   # #tileNeighborsCentralDown<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord-1000,".laz")
#   # tileNeighborsCenterDownX<-tileNumberXCoord
#   # tileNeighborsCenterDownY<-tileNumberYCoord-1000
#   # df<-loadTile(path,tileNeighborsCenterDownX,tileNeighborsCenterDownY)
#   # df<-makeSpatialDF(df,projection)
#   # df7<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymin(extensionMainTile)-maxView, ymin(extensionMainTile)))
#   #
#   # #tileNeighborsCentralUp<-paste0("ahn_", tileNumberXCoord,"_",tileNumberYCoord+1000,".laz")
#   # tileNeighborsCenterUpX<-tileNumberXCoord
#   # tileNeighborsCenterUpY<-tileNumberYCoord+1000
#   # df<-loadTile(path,tileNeighborsCenterUpX,tileNeighborsCenterUpY)
#   # df<-makeSpatialDF(df,projection)
#   # df8<-crop(df,c(xmin(extensionMainTile),xmax(extensionMainTile),ymax(extensionMainTile), ymax(extensionMainTile)+maxView))
#   #
#   dfs<-c(df1)#,df2)
# }
# 


makeRaster<-function(spatialDF, xres, yres, pro){
  dummyRaster<-raster(nrow=10,ncol=10,crs=pro) #dummy raster with projection
  extent(dummyRaster)<-extent(spatialDF)
  res(dummyRaster)<-c(xres,yres) # set the resolution
  r<-rasterize(spatialDF,dummyRaster,field="Z") #rasterizing the spatial points to a 1x1 grid
  r
}



SVF<-function(pointX, pointY, maxView, proj){
  
  
  tileNumberXCoord<-floor(pointX/1000)*1000
  tileNumberYCoord<-floor(pointY/1000)*1000
  
  
  
  mainTile<-loadTile(lazFolder, tileNumberXCoord, tileNumberYCoord)
 
  mainTile<-makeSpatialDF(mainTile,projection = pro)
  extensionMainTile<-extent(mainTile)

  neighbors<-loadNeighborTiles(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)
  #neighbors<-loadNeighborTilesTest(lazFolder, tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView, pro)

  rasterizedNeighbors<-lapply(neighbors, makeRaster, Xres, Yres, pro)
  mergedNeighbors<-do.call(merge, rasterizedNeighbors)
  rm(neighbors)
  rm(rasterizedNeighbors)
  rasterizedMainTile<-makeRaster(mainTile,Xres,Yres,pro)
  rm(mainTile)
  fullRaster<-merge(rasterizedMainTile, mergedNeighbors)
  rm(mergedNeighbors)
  gc()

  r.svf<-svf(fullRaster, nAngles=16, maxDist= maxView, ll=F)
  out<-crop(r.svf,extent(rasterizedMainTile))


  # plot(r.svf)
  r.b<-brick(rasterizedMainTile,out)
  names(r.b)<-c("Z","SVF")
  r.df<-as.data.frame(r.b,xy=TRUE)

  #r.df2<-r.df[complete.cases(r.df),]

  #cells<-ncell(r.svf)
  #write.table(cells,file="/nobackup/users/pagani/cells.txt",row.names=FALSE,col.names=FALSE,append=TRUE)
  writeRaster(r.b,filename=paste0("/home/pagani/development/SkyViewFactor/data/gridsSVF/",
                                  tileNumberXCoord, "_",tileNumberYCoord,".grd"),
                                  format="raster",
                                  overwrite=TRUE)
  write.table(r.df,file="testSVF.txt",sep=",",row.names = FALSE, append = TRUE, col.names = !file.exists("testSVF.txt"))
  return("file written")
}




