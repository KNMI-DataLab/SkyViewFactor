#runApp("inst/shiny/")
library(shiny)
library(raster)
library(horizon)
library(leaflet)
library(rgdal)
library(stringi)
library(stringr)
library(leaflet)
library(DT)
library(shinyjs)

pro=CRS("+init=epsg:28992")
WGS84<-CRS("+init=epsg:4326")

coords<-readRDS("~/SVF/SkyViewFactor/inst/extdata/coordsKNMI.rds")
coords<-coords[complete.cases(coords),]
names(coords)<-c("lat","lon","DS_CODE")
coords<-data.frame(coords)
coordinates(coords)<-~lon+lat
projection(coords)<-WGS84
coords.pro<-spTransform(coords,pro)
coords.pro<-data.frame(coords.pro)

rfile<-"/home/pagani/development/SkyViewFactor/data/gridsNLSVF/140000_456000.grd"
st<-stack(rfile)
st<-projectRaster(st,crs=WGS84)
box<-bbox(st)
#lf<-leaflet() %>% addTiles() %>% addRasterImage(st[[2]], colors = "Spectral",opacity = 0.8)   



server<-function(input,output,session){
  #addClass(selector = "body", class = "sidebar-collapse") # to collapse sidebar
  
  output$coords = DT::renderDataTable(coords.pro, selection = 'single')
 
  selectedPoint<-reactive({
    coords.pro[input$coords_row_last_clicked,]
  })
  selectedTiles <- reactive({
    AWSpoint<-selectedPoint()
    datafile<-point_info_from_grid(point=AWSpoint,n=2)
    st<-stack(datafile$grid.file)
    projectRaster(st,crs=WGS84)
    
    
  })
  
  output$svf<-renderLeaflet({
    
    if(is.null(input$coords_row_last_clicked)){dat<-st
    } else {dat <- selectedTiles()
           }
    box<-bbox(dat)
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(dat[[2]]),
                        na.color = "transparent")
    
    leaflet() %>% addTiles() %>% fitBounds(box[1],box[2],box[3],box[4]) %>% 
      addRasterImage(dat[[2]], colors = pal,opacity = 0.8) %>% 
      addMarkers(data=coords, popup = ~ DS_CODE) %>%
      addLegend("bottomright",title="Sky View Factor",pal = pal,values=values(dat[[2]]),opacity = 1)
    #%>%fitBounds(lat1 = box[1], lng1 = box[2], lat2 = box[3], lng2 = box[4])
  })
}