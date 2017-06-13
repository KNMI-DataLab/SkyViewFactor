#coordinates box
#Den Haag  51.7-52.4 N, 3.8-4.9 E. 
#Eindhoven 51.1-51.9 N, 5-6 E.
library(R.utils)
library(magrittr)
library(stringr)
library(RCurl)
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
EindhovenTiles<-readRDS("data/ReinderEindhoven.rds")

#copy data with RCurl
#scp() #some code to copy


##Checking if the suppoosed GRD files are computed already or those tiles do not exist (e.g., sea, Germany, Belgium)
wholeNLSVFData<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/"

#files<-lapply(HaagTiles,list.files, path = wholeNLSVFData, full.names = T)


#tilesAvailableHaag<-sum(unlist(lapply(files, function(x) !identical(x,character(0)))))





#get laz files and clean the filename ahn_ and _1,2... to compare with computed
lazFolder <- c("/data1/", "/data2/", "/data3")
listLAZTiles <- list.files(path = lazFolder, ".laz", full.names = T, recursive = T)
test11<-listLAZTiles %>% str_replace("_[1,2,3,4,5,6].laz",".laz")
lazRaw<-lapply(test11, function(x) str_replace(str_replace(tail(unlist(str_split(x,patter = "/")),1),pattern = ".laz",""),"ahn_",""))
removedUndescoreTiles<-unique(lazRaw)



#clean extention of files that are in the Haag Eindoven extentions
rawTilesNamesHaagToHave<-HaagTiles %>% str_replace(".grd", "")
rawTilesNamesEindhovenToHave<-EindhovenTiles %>% str_replace(".grd", "")



#number of tiles to be processed for Haag Eindhoven that overlap with Laz (sea, Belgium, Germany are not needed)
totalLazForHaag<-sum(rawTilesNamesHaagToHave %in% removedUndescoreTiles)
totalLazForEindhoven<-sum(rawTilesNamesEindhovenToHave %in% removedUndescoreTiles)
lazToBeProcessedForHaag<-rawTilesNamesHaagToHave[rawTilesNamesHaagToHave %in% removedUndescoreTiles]
lazToBeProcessedForEindhoven<-rawTilesNamesEindhovenToHave[rawTilesNamesEindhovenToHave %in% removedUndescoreTiles]


#get the files that are already computed
computedFiles<-list.files(wholeNLSVFData, ".grd")
computedAvailableForHaag<-sum(HaagTiles %in% computedFiles)
computedAvailableForEindhoven<-sum(EindhovenTiles %in% computedFiles)
computedFiles<-computedFiles %>% str_replace(".grd", "")


#compare the computed with the one to be computed
stillToComputeEindhoven<-lazToBeProcessedForEindhoven[!lazToBeProcessedForEindhoven %in% computedFiles]
stillToComputeHaag<-lazToBeProcessedForHaag[!lazToBeProcessedForHaag %in% computedFiles]

length(stillToComputeEindhoven)



 
# if(computedAvailableForHaag==totalLazForHaag){
#   message("Haag is fully computed")
# } else{
#   message("Haag not fully computed")
# }
# 
# if(computedAvailableForEindhoven==totalLazForEindhoven){
#   message("Eindhoven is fully computed")
# } else{
#   message("Einhoven not fully computed")
# }

