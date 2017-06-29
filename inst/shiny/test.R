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
#lf<-leaflet() %>% addTiles() %>% addRasterImage(st[[2]], colors = "Spectral",opacity = 0.8)   



server<-function(input,output,session){
  
  
  output$coords = DT::renderDataTable(coords.pro, selection = 'single')
#     datatable(coords.pro, rownames = FALSE, selection = 'single', 
#     callback = JS("table.on('click.dt', 'td', function() {
#             var row_=table.cell(this).index().row;
#             var col=table.cell(this).index().column;
#             var rnd= Math.random();
#             var data = [row_, col, rnd];
#             Shiny.onInputChange('rows',data );
# });"))
#     
#     )
    
  selectedTiles <- reactive({
    AWSpoint<-coords.pro[input$coords_row_last_clicked,]
    datafile<-point_info_from_grid(point=AWSpoint,n=2)
    stack(datafile$grid.file)
    
   
  })
  
   output$svf<-renderLeaflet({
    
    if(is.null(input$coords_row_last_clicked)){dat<-st
    } else {dat <- selectedTiles()}
   
    leaflet() %>% addTiles() %>% addRasterImage(dat[[2]], colors = "Spectral",opacity = 0.8)     
  })
}
ui<-fluidPage(title="SVF",fluidRow(leafletOutput("svf")),
                          fluidRow(DT::dataTableOutput("coords")))

shinyApp(ui, server)

# st<-eventReactive(input$coords_row_clicked,{
#   AWSpoint<-coords.pro[input$coords_row_clicked,]
#   datafile<-point_info_from_grid(point=AWSpoint,n=2)
#   stack(datafile$grid.file)
# })
#   observeEvent(input$coords_row_clicked,{ #Observer to edit colors and valid range
#     
#     
#       leafletProxy("svf",session) %>%
#       clearTiles() %>%
#       clearImages() %>%
#       leaflet() %>% addTiles() %>%
#       addRasterImage(selectedTiles[[2]], colors = "Spectral",opacity = 0.8) 
#     })

