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
library(leaflet.extras)
library(ggmap)
library(oce)
library(raster)

load(file='./polygon.RData')
df_city <- fread('region.csv')
xy <- df_city[, c('Longitude', 'Latitude')]


df_city <- SpatialPointsDataFrame(coords = xy, data = df_city,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>>%
  st_as_sf

risk <- function(A, gas, s_hour, s_level){
  
  level <- c(0, 50, 250, 500, 1000, 2000, 3000, 5000)
  time <- seq(0, 24, 1)
  
  if(gas == 'PM25'){
    A <- A[,,which(level == s_level), which(time == s_hour)]
    return(A/25)
  } else if(gas == 'SO2'){
    A <- A[,,which(level == s_level), which(time <= s_hour)]
    
    return(apply(A >= 350, c(1, 2), mean))
    
    
  } else if(gas == 'NO2') {
    A <- A[,,which(level == s_level), which(time <= s_hour)]
    
    return(apply(A >= 200, c(1, 2), mean))
  } else if(gas == 'PM10') {
    A <- A[,,which(level == s_level), which(time == s_hour)]
    return(A/40)
  } else{
    
    A <- A[,,which(level == s_level), which(time <= s_hour)]%>>%
         apply(c(1, 2), max)
    
    
    if(gas == 'CO'){ 
      return(A/10)
    } else{
        return(A/120)
      }
    
  }
}


#
open_file <- function(folder, gaz, period, date){
  part_1 <- "W_fr-meteofrance,MODEL,ENSEMBLE+FORECAST+ALLLEVELS+"
  part_2 <- paste(gaz, "+", sep = "", collapse = NULL)
  part_3 <- period
  part_4 <- "_C_LFPW_"
  part_5 <- date
  part_6 <- ".nc"
  
  res <- paste(folder,
               part_1,
               part_2,
               part_3,
               part_4,
               part_5,
               part_6,
               sep = "", collapse = NULL)
  return(res)
}

make_plot <- function(df_tmp){
  df_tmp %>>%
  {
    . %>>% mutate(txt=paste('Country: ', Country,
                            '</br> Region: ', Name,
                            '</br> Measure: ', v)) %>>%
      ggplot(aes(Name, v)) + geom_col(aes(fill=Country), alpha=0.5) + 
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank(),
            legend.position="none") +
      ylab('Measure') + 
      coord_flip()
    
  } %>>% ggplotly()
  
}




function(input, output, session) {
  
  output$region <- renderUI({
    cities <- df_city %>>% filter(Country %in% input$select_country) %>>% {.$Name}
    
    selectInput("select_region", h3("Region"), multiple = T,choices = cities)
  })
  
  
  
  df_forcast <- eventReactive(input$change_file, {
    file_path <-  open_file(folder="./Forecast/",
              gaz= input$select_gas,
              period= input$select_time,
              date = input$select_date)
    nc_file <- nc_open(file_path)

    return(nc_file)
  })
  
  df_heatmap <- eventReactive(input$submit | input$change_file, {
    nc_file <- df_forcast()
    level <- c(0, 50, 250, 500, 1000, 2000, 3000, 5000)
    time <- seq(0, 24, 1)
    var <- ncvar_get(nc_file, nc_file$var[[1]]$name)
    
    if(input$select_scale == 'Original value'){
      A <- var[seq(1, 700, by = 5), seq(1, 400, by = 5), which(level == input$select_level), which(time == input$select_hour)]
    } else if(input$select_scale == 'Max') {
      
      if(length(which(level <= input$select_level))>1){
        A <- var[seq(1, 700, by = 5), seq(1, 400, by = 5), which(level == input$select_level), which(time <= input$select_hour)] %>>%
          apply(c(1, 2), max)
      } else {
        A <- var[seq(1, 700, by = 5), seq(1, 400, by = 5), which(level == input$select_level), which(time == input$select_hour)] 
      }
      
      
      
    } else {
      A <- risk(var[seq(1, 700, by = 5), seq(1, 400, by = 5),,], input$select_gas, input$select_hour, input$select_level)
    }
        
    df_heatmap <- matrixSmooth(A, passes = 3) %>>%
      as.data.table
    
    df_heatmap$lon <- seq(1, 700, by = 5)
    df_heatmap <- melt(df_heatmap, id='lon', variable.name ='lat', value.name = 'value')

    return(df_heatmap$value)
  })
  
  
  
  r_map <- reactive({
    
    p$v <- df_heatmap()
    mypalette <- colorNumeric(palette="viridis", domain=p$v, na.color="transparent")
    map_plot <- leaflet(p) %>>% addTiles() %>>%
      addPolygons(opacity = 0,
                  color = 'white',
                  weight = .25,
                  fillOpacity = 0.5,
                  fillColor = ~mypalette(v),
                  smoothFactor = 0) %>>%
      fitBounds(-25, 30, 45, 70) %>>%
      addLegend(pal=mypalette, values=~v, opacity=0.7, title = "Measure", position = "bottomleft")
    
    return(map_plot)
    
  })
  
  
  markers <- eventReactive(input$locate | input$submit | input$change_file, {
    p$v <- df_heatmap()
    df_tmp <- df_city %>>% filter(Name %in% input$select_region)
    df_tmp <- st_join(df_tmp, p)
    mytext=paste("Country: <b>", df_tmp$Country,"</b><br/>", 
                 "Region: <b>", df_tmp$Name, "</b><br/>", 
                 input$select_scale, " of ", input$select_gas, ": <b>", round(df_tmp$v, 4), "</b>",
                 sep="") %>>%
      lapply(htmltools::HTML)
    
    
    return(list(table = df_tmp, text=mytext))
  })
  
  
  output$bar <- renderPlotly({
    df <- markers()
    
    return(make_plot(df$table))
  })
  output$map <- renderLeaflet({
    map_plot <- r_map()
    
    if(input$locate){
      df_list = markers()
      map_plot %>>% addMarkers(data=df_list$table, lng=~Longitude, lat=~Latitude, label=~df_list$text)
    } else{
      map_plot
    }
    
   })
  
  
  
  
  
}




