library(shiny)
library(leaflet)
library(DT)
library(shinydashboard)



ui<-dashboardPage(
  dashboardHeader(title = div(img(height = 50, 
                                  src = "datalab_logo.jpg", 
                                  class = "pull-lef"),
                              "Sky View Factor")),
  dashboardSidebar(
    h3("Get started!"),
    p("Explore the Sky View Factor (SVF) for the 32AWS location. Choose a location from the table and explore the site. Can you find buildings, trees 
      and other landscape features?"),
    h3("Background"),
    tags$img(src="SVF2D.jpg", width=200),
    # div(img(height = 50,scr="SVF2D.jpg", 
    #     class = "pull-lef")),
    p("The SVF is calculated from the AHN2 point cloud. The irregular points are rasterized into a grid of 5x5 meter. For the
      calculations of the SVF the R package horizon is used. A radius of 100 meter and 16 different directions were used."),
    h3("Computational challenges"),
    p("The total dataset we started with was 1.5TB. After computing we reduced this to 66GB. To process this amount of data the project used parallel
      processing on 10 cores.")
    
    
    
  ),
  dashboardBody(
    #useShinyjs(), # to collapse sidebar
    fluidPage(
    
    fluidRow(
      
      column(4,DT::dataTableOutput("coords")),
             column(4,leafletOutput("svf"))
    )
     
)
)
)