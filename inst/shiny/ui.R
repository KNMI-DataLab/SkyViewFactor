library(shiny)
library(leaflet)
library(DT)



ui<-fluidPage(titlePanel("Sky View Factor"),
              
              fluidRow(leafletOutput("svf")),
              fluidRow(DT::dataTableOutput("coords")))