#coordinates box
#Den Haag  51.7-52.4 N, 3.8-4.9 E. 
#Eindhoven 51.1-51.9 N, 5-6 E.
pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")


Haag<-extent(3.8,4.9,51.7,52.4)
Eindhoven<-extent(5,6,51.1,51.9)

Haag.r<-raster(Haag)
crs(Haag.r)<-WGS84

Haag.r<-as(Haag.r,"SpatialPointsDataFrame")
Haag.r<-spTransform(Haag.r,pro)

ext<-extent(Haag.r)

topleft<-SpatialPoints(cbind(ext[1],ext[4]), proj4string=pro)
topright<-SpatialPoints(cbind(ext[2],ext[4]), proj4string=pro)
downleft<-SpatialPoints(cbind(ext[1],ext[3]), proj4string=pro)
downright<-SpatialPoints(cbind(ext[2],ext[3])  , proj4string=pro)

diff.top<-topright@coords-topleft@coords
xdiff<-diff.top[1]
xnr.tiles<-ceiling(xdiff/1000)

diff.bottom<-topright@coords-downright@coords
ydiff<-diff.bottom[2]
ynr.tiles<-ceiling(ydiff/1000)

#Get the first tile (topleft)
tl<-data.frame(topleft)
names(tl)<-c("x","y")

tileNumberXCoord<-floor(tl$x/1000)*1000
tileNumberYCoord<-floor(tl$y/1000)*1000

toprow.xtiles<-seq(tileNumberXCoord,tileNumberXCoord+xnr.tiles*1000,by=1000)
toprow.ytiles<-rep(tileNumberYCoord,length(xtiles))
toprow<-cbind(toprow.xtiles,toprow.ytiles)

#tiles_unique<-unique(point[c("tileNumberXCoord","tileNumberYCoord")])
#tiles_unique_names<-paste0(str_pad(as.integer(tiles_unique$tileNumberXCoord), width = 6, pad = "0"),"_",
#                           str_pad(as.integer(tiles_unique$tileNumberYCoord),  width = 6, pad = "0"), ".grd")  
#plot(Haag.r)
#points(topleft,col="red")
#points(topright,col="blue")
#points(downright,col="orange")
#points(downleft,col="green")