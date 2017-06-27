library(raster)
pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")
#GMS data
file.points<-fread("data/allstations_edit.txt")
names(file.points)<-c("name","lat","lon","netwerk")

coordinates(file.points)<-~lon+lat
crs(file.points)<-WGS84
file.points<-spTransform(file.points,CRSobj=pro)

df<-data.frame(file.points)

#why does this work?
for (i in 1:length(df$name)){
point_info_from_grid(point=df[i,])
}
# 
# #and this one not? :(
# lapply(df,point_info_from_grid)