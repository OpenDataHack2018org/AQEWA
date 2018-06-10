library(maptools)
require(rgdal)
library(ggplot2)
library(maps)
library(rworldmap)
library(shiny)
library(leaflet)
library(plotly)
library(geojsonio)
library(rmapshaper)
library(data.table)
library(crosstalk)
library(reshape2)
library(magrittr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tidyr)
library(purrr)
library(pipeR)
library(stringi)
library(stringr)
library(lazyeval)
library(rCharts)
library(xml2, warn.conflicts = FALSE)
library(rlist, warn.conflicts = FALSE)
library(fasttime, warn.conflicts = FALSE)
library(mongolite, warn.conflicts = FALSE)
library(ggplot2)
library(parallel)
library(doSNOW)
library(foreach)
library(sf)
library(maptools)
library(ncdf4)
require(rgdal)
library(shinythemes)
library(leaflet)
library(shinydashboard)

df_city <- fread('region.csv')

data_list <- list.files('./Forecast') %>>%
  str_extract('\\+.*(nc)$') %>>%
  str_replace_all('\\+FORECAST\\+ALLLEVELS\\+|\\.nc', '') %>>%
  str_split('\\+|_') %>>%
  llply(function(x){
    return(x[c(1, 2, 5)])
  }) %>>% transpose() %>>% simplify_all() %>>%
  as.data.table() %>>%
  setnames(names(.), c('gas', 'time', 'date'))

header <- dashboardHeader(
  title = "Air Quality"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
     menuItem("Overview", tabName = "heatmap", icon = icon("map")),
    menuItem("Historical", tabName = "historical", icon = icon("calendar")),
    menuItem("Summary", tabName = "summary", icon = icon("align-left")),
    menuItem("Process Monitor", tabName = "process_monitor", icon = icon("eye")),
    menuItem("Download", tabName = "download", icon = icon("folder-open"))
    
    
    
  )
)


body <- dashboardBody(
  
  fluidRow(
     column(width = 9,
            status = "warning",
            
            box(width = NULL, solidHeader = TRUE,
                tabItems(
            
                  tabItem(tabName = "heatmap",
                          leafletOutput("map", height = 1050)
                  ),
                  tabItem(tabName = "historical",
                          h2("Historical")),
                  
                  tabItem(tabName = "summary",
                          plotlyOutput("bar", height = 1050)
                          )
                  
                )
              
               )
            ),

     

     
    column(width = 3,
           box(width = NULL, status = "warning",
               selectInput("select_date", h3("Date"), 
                           choices = unique(data_list$date), selected = 1),
               selectInput("select_gas", h3("Gas"),
                            choices =      list("CO (max=10 mg/m3 per 8 hour)"="CO", 
                                                "NO2 (max= 200 micron-g/m3 per hour)"="NO2", 
                                                "O3 (max= 120 micron-g/m3 per 8 hour)"="O3", 
                                                "PM10 (max= 50 micron-g/m3 per 24 hour)"="PM10", 
                                                "PM2.5 (max= 25 micron-g/m3 per year)"="PM25", 
                                                "SO2 (max= 350 micron-g/m3 per hour)"="SO2"), selected = 1),
               selectInput("select_time", h3("Time"),
                           choices = unique(data_list$time), selected = 1),
               actionButton("change_file", label = "Loading", icon = icon("circle-o-notch"), class="btn-info")
               
           ),
           box(width = NULL, status = "warning",
               
               
               numericInput("select_hour", 
                            h3("Hour (between 0 and 24)"), 
                            value = 0),
               selectInput("select_level", h3("Level (m)"),
                           choices = c(0, 50, 250, 500, 1000, 2000, 3000, 5000)),
               selectInput("select_scale", h3("Measure"),
                           choices = c('Concentrations (micron-g/m^3)'='Original value','Max'= 'Max','Risk of exceeding'= 'Risk')),
               actionButton("submit", label = "Submit", icon = icon("play"), class="btn-info")
               
           ),
          
           box(width=NULL, status = "warning",
               selectInput("select_country", h3("Country"), multiple = T,
                           choices = unique(df_city$Country)),
               uiOutput("region"),
               actionButton("locate", label = "Locate", icon = icon("map-marker"), class="btn-info")
               )
    )
  )

)

dashboardPage(
  header,
  sidebar,
  body
)