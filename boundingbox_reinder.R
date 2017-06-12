#coordinates box
#Den Haag  51.7-52.4 N, 3.8-4.9 E. 
#Eindhoven 51.1-51.9 N, 5-6 E.
library(R.utils)
sourceDirectory("functions")

pro<-CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")


Haag<-extent(3.8,4.9,51.7,52.4)
Eindhoven<-extent(5,6,51.1,51.9)

Haag.ext<-extent_from_new_pro(Haag)
Eindhoven.ext<-extent_from_new_pro(Eindhoven)

files.Haag<-grid_file_name_from_extent(Haag.ext)
files.Eindhoven<-grid_file_name_from_extent(Eindhoven.ext)

saveRDS(files.Haag,"data/ReinderHaag.rds")
saveRDS(files.Eindhoven,"data/ReinderEindhoven.rds")



HaagTiles<-readRDS("data/ReinderHaag.rds")
EindhoveTiles<-readRDS("data/ReinderEindhoven.rds")



##Checking if the suppoosed GRD files are computed already or those tiles do not exist (e.g., sea, Germany, Belgium)
wholeNLSVFData<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"

#files<-lapply(HaagTiles,list.files, path = wholeNLSVFData, full.names = T)


#tilesAvailableHaag<-sum(unlist(lapply(files, function(x) !identical(x,character(0)))))

computedFiles<-list.files(wholeNLSVFData, ".grd")
computedAvailableForHaag<-sum(HaagTiles %in% computedFiles)
computedAvailableForEindhoven<-sum(EindhoveTiles %in% computedFiles)




lazFolder <- c("/data1/", "/data2/", "/data3")


listLAZTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)

rawTilesNamesHaag<-HaagTiles %>% str_replace(".grd", "")
rawTilesNamesEindhoven<-EindhoveTiles %>% str_replace(".grd", "")




test11<-listLAZTiles %>% str_replace("_[1,2,3,4,5,6].laz",".laz")

lazRaw<-lapply(test11, function(x) str_replace(str_replace(tail(unlist(str_split(x,patter = "/")),1),pattern = ".laz",""),"ahn_",""))
removedUndescoreTiles<-unique(lazRaw)

totalLazForHaag<-sum(rawTilesNamesHaag %in% lazRaw)
totalLazForEindhoven<-sum(rawTilesNamesEindhoven %in% lazRaw)

 
if(computedAvailableForHaag==totalLazForHaag){
  message("Haag is fully computed")
} else{
  message("Haag not fully computed")
}

if(computedAvailableForEindhoven==totalLazForEindhoven){
  message("Eindhoven is fully computed")
} else{
  message("Einhoven not fully computed")
}

