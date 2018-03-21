library(data.table)
library(ggplot2)
library(raster)
library(rasterVis)
svf.palette<-colorRampPalette(c("purple","blue","green","yellow","red"))
cfg <- config::get(file = "/nobackup/users/dirksen/Temperature/GeoInterpolation/config/config.yml")
main_dir<-"/net/bhw510/nobackup/users/pagani/SVFprocessedGrids/deBiltTests/"

deBilt_res<-list.files(paste0(main_dir,"otherResolutions"),pattern="./*.grd",full.names = TRUE)

deBilt_1m<-list.files(paste0(main_dir,"1m"),pattern="./*.grd", full.names = TRUE)
deBilt_1m.names<-list.files(paste0(main_dir,"1m"),pattern="./*.grd", full.names = FALSE)
deBilt_1m.names<-gsub("1m_svf","",deBilt_1m.names)
deBilt_1m.names<-gsub(".grd","",deBilt_1m.names)
deBilt_1m.names.radius<-gsub("_../*","",deBilt_1m.names)
deBilt_1m.names.directions<-gsub(".../*_","",deBilt_1m.names)

deBilt_5m<-list.files(paste0(main_dir,"5m"),pattern="./*.grd", full.names = TRUE)

sensitivity_radius_directions<-list.files("/net/bhw510/nobackup/users/pagani/SVFprocessedGrids/deBiltTests/1m/latestSensAnalysis/",
                                          pattern=".grd",
                                          full.names = TRUE)
###################################
st.1m<-stack(deBilt_1m[1])
names(st.1m)<-"res_1m"

st.5m<-stack(deBilt_5m[1])
names(st.5m)<-"res_5m"

st.res<-lapply(deBilt_res,stack)
resolution<-paste0("res_",lapply(st.res,xres),"m")


st.res[[length(st.res)+1]]<-st.1m
st.res[[length(st.res)+1]]<-st.5m
names(st.res)<-c(resolution,"res_1m","res_5m")


################################compare different resolution and their correlations
st.m<-st.res


raster_to_new_res<-function(layer,layer_new_res=st.m$res_10m){
r<-resample(layer,layer_new_res,"bilinear")
return(r)
}

st.new<-lapply(st.m,FUN=raster_to_new_res)

st<-stack(st.new)
st<-dropLayer(st,c(2,6))

st.test<-subset(st,order(c(6,2,3,4,1,5)))
# r.c<-corLocal(r1,r2,method='spearman')
p.c<-layerStats(st.test,'pearson',na.rm=TRUE)
corr_matrix=p.c$'pearson correlation coefficient'

library(corrplot)

colnames(corr_matrix)<-c("1m","2m","3m","4m","5m","10m")
rownames(corr_matrix)<-c("1m","2m","3m","4m","5m","10m")

# png("/usr/people/dirksen/Pictures/SVF/correlations.png",width=1800,height=1800,res=300)
corrplot(corr_matrix,
         is.corr=FALSE,
         type="lower",
         diag = FALSE,
         method="square",
         col=colorRampPalette(c("blue","purple","red","orange","yellow","green","cyan"))(200),
         tl.col = "black",
         insig="p-value",
         sig.level=-1,
         p.mat=corr_matrix,
         tl.srt = 45)
# dev.off()
#layerStats()

p<-levelplot(st.test,
             layout=c(2,3),
             scales=list(draw=FALSE),
             col.regions=svf.palette(20),
             at = seq(0,1,length=20))
# png("/usr/people/dirksen/Pictures/SVF/resolutions.png",width=1800,height=2400,res=300)
# print(p)
# dev.off()
################################### SENSITIVITY RADIUSSES AND DIRECTIONS
I<-which(deBilt_1m.names.directions=="16")
srd.1m<-stack(deBilt_1m[I])
srd<-stack(sensitivity_radius_directions)
srd<-stack(srd,srd.1m)

ext.zoom<-as(raster::extent(matrix(c(xmin=5.17,ymin=52.12,xmax=5.19,ymax=52.13),nrow=2)),"SpatialPolygons")
proj4string(ext.zoom)<-cfg$WGS84
ext.zoom.rd<-spTransform(ext.zoom,cfg$pro)
srd<-crop(srd,ext.zoom.rd)
#srd<-raster_to_new_res(srd)

sens.names<-list.files("/net/bhw510/nobackup/users/pagani/SVFprocessedGrids/deBiltTests/1m/latestSensAnalysis/",pattern=".grd")

sens.names<-gsub(".grd","",sens.names)
radiuss<-gsub("1m_svf","",sens.names)
radiuss<-gsub("_[0-9]+","",radiuss)

directions<-gsub("1m_svf","",sens.names)
directions<-gsub("[0-9]+_","",directions)

D<-deBilt_1m.names.directions[I]
R<-deBilt_1m.names.radius[I]
radiuss<-c(radiuss,R)
directions<-c(directions,D)




R<-sort(as.numeric(unique(radiuss)))
df<-data.frame("radiuss"=radiuss,"directions"=directions,"I"=seq(1:length(radiuss)))
df$radiuss<-as.character(df$radiuss)
df$directions<-as.character(df$directions)
df<-df[which(df$directions!="1" & df$directions!="3"),]

for(i in 1:length(R)){
d1<-which(df$radiuss==R[i])
srd_R<-srd[[df$I[d1]]]
names(srd_R)<-paste0("directions_",df$directions[d1])
srd_R<-subset(srd_R,order(as.numeric(df$directions[d1])))

p<-levelplot(srd_R,
             layout=c(2,2),
             scales=list(draw=FALSE),
             col.regions=svf.palette(20),
             at = seq(0,1,length=20))
png(paste0("/usr/people/dirksen/Pictures/SVF/direction_sensitivity_",R[i],".png"),width=1800,height=1700,res=300)
print(p)
dev.off()
}
################################### Get map with the exent of the layers
library(ggmap)
library(ggplot2)
r<-st[[1]]
r.wgs<-projectRaster(r,crs=cfg$WGS84)
# ext<-extent(r.wgs)
# map<-get_map(location=c(lon=((ext@xmax+ext@xmin)/2),lat=((ext@ymax+ext@ymin)/2)),zoom=12,maptype = "terrain")

ext.zoom<-extent(matrix(c(xmin=5.17,ymin=52.12,xmax=5.19,ymax=52.13),nrow=2))
map.zoom<-get_map(location=c(lon=((ext.zoom@xmax+ext.zoom@xmin)/2),lat=((ext.zoom@ymax+ext.zoom@ymin)/2)),zoom=14,maptype = "satellite")

m<-ggmap(map.zoom) + 
  scale_x_continuous(limits = c(ext.zoom@xmin,ext.zoom@xmax),expand=c(0,0)) +
  scale_y_continuous(limits = c(ext.zoom@ymin,ext.zoom@ymax),expand = c(0,0))
m
###################################



# png.to.write<-paste0("/usr/people/dirksen/Pictures/SVF/",names(st.res),".png")

svf.plot<-function(r.plot,png.to.write){

png(png.to.write,width=1800,height=1800,res=300)
p<-levelplot(r.plot,
             scales=list(draw=FALSE),
             col.regions=svf.palette(20),
             at = seq(0,1,length=20))
print(p)
dev.off()
}

# mapply(svf.plot,st.res,png.to.write)

##########################1m sensitivity
st.1m<-stack(deBilt_1m)

st.1m_new<-raster_to_new_res(st.1m)
p.c<-layerStats(st.1m_new,'pearson',na.rm=TRUE)
corr_matrix=p.c$'pearson correlation coefficient'

st.10016<-st.1m[[1]]
st.80064<-st.1m[[12]]
st.diff<-st.10016-st.80064
st.compare<-stack(st.10016,st.80064)
names(st.compare)<-c("Radius_100m_Directions_16","Radius_800m_Directions_64")


p<-levelplot(st.compare,
             layout=c(2,1),
             scales=list(draw=FALSE),
             col.regions=svf.palette(20),
             at = seq(0,1,length=20))
# png("/usr/people/dirksen/Pictures/SVF/compare.png",width=1800,height=900,res=300)
print(p)
# dev.off()

# png("/usr/people/dirksen/Pictures/SVF/diff_compare.png",width=1800,height=1800,res=300)
print(p)
# dev.off()
svf.palette<-colorRampPalette(c("purple","white","red"))
p<-levelplot(st.diff,
             scales=list(draw=FALSE),
             col.regions=svf.palette(30))

##########################system.time
benchmark_5m_deBilt<-fread(paste0(main_dir,"benchmarkTime/5mResResultsTime.csv"))

benchmark_5m_deBilt$relative.time<-benchmark_5m_deBilt$time/max(benchmark_5m_deBilt$time)*100
benchmark_5m_deBilt$directions<-gsub("svf..._/*","",benchmark_5m_deBilt$expr)
benchmark_5m_deBilt$radius<-matrix(unlist(strsplit(gsub("_../*","",benchmark_5m_deBilt$expr),"svf")),ncol=2,byrow=TRUE)[,2]

benchmark_5m_deBilt$directions<-as.numeric(benchmark_5m_deBilt$directions)
benchmark_5m_deBilt$radius<-as.numeric(benchmark_5m_deBilt$radius)

png("/usr/people/dirksen/Pictures/SVF/benchmark_time.png",width=1800,height=1200,res=300)
ggplot(benchmark_5m_deBilt,aes(relative.time,radius,colour = factor(directions))) + 
  geom_point() + 
  stat_smooth(method="lm",size=0.5) +
  scale_colour_discrete(name="Number of\nDirections") +
  ylab("Radius [m]") +
  xlab("Time [%]")
dev.off()

# benchmark_resolution<-fread(paste0(maindir,"otherResolutions/"))
expr<-c("svf100_16_1m","svf100_16_2m","svf100_16_3m","svf100_16_4m","svf100_16_5m","svf100_16_10m","svf100_16_20m","svf100_16_50m")
res<-c(1,2,3,4,5,10,20,50)
min<-c(547.12984,132.97750,86.52716,72.89858,66.69391,58.24067,55.08935,52.95532)
lq<-c(547.97540,133.27085,86.57732,72.93163,66.92816,58.47219,55.18677,53.02377)
mean<-c( 551.00564,135.19952,87.53631,74.01177,67.08959,58.66121,56.05886,53.93315)
median<-c(549.21323,134.31435,86.85888,73.09344,67.08180,58.67860,55.27541,53.11066)
uq<-c(551.60944,135.44233,88.27299,75.90408,67.21456,58.87630,57.06586,55.13121)
max<-c(566.41225,143.93858,90.71153,76.64660,67.77510,59.06577,58.19002,56.39803)  

benchmark_resolution<-data.frame(expr,res,min,lq,mean,median,uq,max)

# png("/usr/people/dirksen/Pictures/SVF/benchmark_resolution.png",width=1800,height=1200,res=300)

ggplot(benchmark_resolution,aes(y=median,x=res)) + 
  geom_point(shape=21, size=2, fill="white") +
  geom_path(linetype=2) +
  scale_y_log10()+
  scale_y_log10(breaks = scales::pretty_breaks())+
  scale_x_log10()+
  scale_x_log10(breaks = scales::pretty_breaks())+
  #coord_trans(x="log10", y="log10") +
  geom_errorbar(aes(ymin=median-(median-min),ymax=median+(max-median)),position=pd)+
  xlab("Resolution [m]") +
  ylab("Time [s]")
dev.off()     
      
            
            
            
            
            
            

                                                                  