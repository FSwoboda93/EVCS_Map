library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(tidyr)
library(plotly)
library(geosphere)
library(magrittr)
library(dplyr)
library(gdistance)
library(Imap)
library(RColorBrewer)
library(sqldf)

source("uimodules/geoDataOutput1.R")
source("uimodules/geoDataOutput2.R")
source("uimodules/geoDataOutput3.R")
source("uimodules/geoDataOutput4.R")
source("uimodules/geoDataOutput5.R")
source("uimodules/geoDataOutput6.R")
source("servermodules/geoData1.R")
source("servermodules/geoData2.R")
source("servermodules/geoData3.R")
source("servermodules/geoData4.R")
source("servermodules/geoData5.R")
source("servermodules/geoData6.R")

source("../2018-ws-mobility-dashboard/functions/pollutionmean.R")
source("../2018-ws-mobility-dashboard/functions/sensormarkers.R")
source("../2018-ws-mobility-dashboard/functions/train_data.R")
source("../2018-ws-mobility-dashboard/functions/osm.R")

ui <- dashboardPage(
  
  
  dashboardHeader(title = tags$a(
    tags$img(src="logo.png", height = "45 px", width ="45 px" ), style ="padding-top: 5 px")
  ),
  
  dashboardSidebar(
    
    selectInput(inputId = "location", "Choose the analyzing location:",
                choices = c("Berlin (BE)" = "Berlin",
                            "Hamburg (HH)" = "Hamburg",
                            "Frankfurt (FM)" = "Frankfurt",
                            "Cologne (CL)" = "Cologne",
                            "Munich (MU)" = "Munich"), selected = "Berlin"),

    
    selectInput(inputId = "period", "Choose the analyzing period\n (Twitter, Luftdaten, Flinkster):",
                choices = c("three-days",
                            "weekly",
                            "monthly",
                            "yearly"), selected = "weekly"),
    
    
    #dateRangeInput(inputId = "period", label = "Choose the analyzing period", min = "2016-01-01"),
    
    sidebarMenu( id="sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
      menuItem("Map", tabName = "map", icon = icon("th"))
    ),
    #Group4: Switch is used to Hide/Show the Twitter minicharts on the map
    switchInput(inputId = "switch", value = F, label = "Twitter minicharts")
    
  ),
  
  dashboardBody(
    
    useShinyjs(),
    
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #ffffff;

                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #ffffff;

                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #ffffff;

                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #2333FF;
                              }'
    ))),
    
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidRow(
                            box( title = "Flinkster",
                                 collapsible=TRUE,
                                 tabBox(
                                   id = "tab1",
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   width = 12,
                                   tabPanel("Overview", geoDataOutput1_2("geoDataGroup1")),
                                   tabPanel("Detailed view on selected period", geoDataOutput1("geoDataGroup1"))
                                 )),
                
                box(
                  title = "OpenStreetMap",
                  width = 6,
                  collapsible=TRUE,
                  geoDataOutput2("geoDataGroup2")
                ),
                
                box(
                  title = "OpenChargeMap",
                  width = 6,
                  collapsible=TRUE,
                  tabBox(id = "Tab1",
                         width = 16,
                         tabPanel("Electric Vehicle Charging Stations", geoDataOutput3_1("geoDataGroup3")),
                         tabPanel("Average Distances between Stations", geoDataOutput3_2("geoDataGroup3")))
                ),
                
                box(
                  title = "Twitter",
                  width = 6,
                  collapsible=TRUE,
                  fluidRow(column(4, dateInput('dateG4',
                                               label = 'Select period start date',
                                               value = "2018-12-10",
                                               min = "2018-12-01",
                                               max = "2019-01-10")),
                           geoDataOutput44("geoDataGroup4"),
                           geoDataOutput43("geoDataGroup4")), 
                          
                  
                  
                  tabsetPanel(type = "tabs", 
                              tabPanel("Tweet frequency", geoDataOutput41("geoDataGroup4")),
                              tabPanel("Wordnet", geoDataOutput42("geoDataGroup4")))
                ),
                
                box(
                  title = "luftdaten.info",
                  width = 6,
                  collapsible=TRUE,
                  geoDataOutput5("geoDataGroup5")

                ),
                
                box(
                  title = "DB Open Data",
                  width = 6,
                  collapsible=TRUE,
                  geoDataOutput6("geoDataGroup6")
                )
              )  ),
      tabItem(tabName = "map", 
              
              box(
                title = "Map",
                height = "100%",
                width = "100%",
                collapsible=TRUE,
                leafletOutput(outputId = "map", height = 600)
              ))
    ))
  
  
    )

server <- function(input, output, session){
  
  
  callModule(geoData1, "geoDataGroup1",input)
  callModule(geoData2, "geoDataGroup2", input)
  df.g3=callModule(geoData3, "geoDataGroup3", input)
  callModule(geoData4, "geoDataGroup4", input)
  callModule(geoData5, "geoDataGroup5", input)
  callModule(geoData6, "geoDataGroup6", input)

  
  coords <- data.frame("loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
                       "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
                       "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))
  
  
  location <- reactive({
    input$location
  })
  
  
  groups <- c("Flinkster rental stations","OpenStreetMap","OpenChargeMap","luftdaten.info","Twitter","DBOpenData")
  
  #Group 6 Train station Data
  berstation <- data.frame(lat = c(53.551086,52.520007,50.110922,52.520007, 50.935173,52.520007, 48.1351253, 52.520007 ),
                 long = c(9.993682, 13.404954,8.682127,13.404954, 6.953101,13.404954, 11.5819806, 13.404954),
                 station = c("Hamburg", "Hamburg", "Frankfurt" , "Frankfurt", "Cologne" ,"Cologne" ,"Munich","Munich" ),
                 info = c ("", paste(info_berhh(input)) , "", paste(info_berffm(input)), "", paste(info_bercol(input)),"",paste(info_bermunch(input))))
  hhstation <- data.frame(lat = c(52.520007, 53.551086, 50.110922, 53.551086, 50.935173, 53.551086, 48.1351253, 53.551086),
                          long = c(13.404954, 9.993682, 8.682127, 9.993682, 6.953101, 9.993682, 11.5819806, 9.993682),
                          station = c("Berlin", "Berlin","Frankfurt","Frankfurt","Cologne","Cologne","Munich","Munich"),
                          info = c ("", paste(info_berhh(input)),"", paste(info_hhffm(input)),"", paste(info_hhcol(input)),"", paste(info_hhmunch(input))))
  ffmstation <- data.frame(lat = c(52.520007, 50.110922, 53.551086, 50.110922, 50.935173, 50.110922, 48.1351253, 50.110922),
                          long = c(13.404954, 8.682127, 9.993682, 8.682127, 6.953101, 8.682127, 11.5819806, 8.682127),
                          station = c("Berlin", "Berlin","Hamburg","Hamburg","Cologne","Cologne","Munich","Munich"),
                          info = c ("", paste(info_berffm(input)),"", paste(info_hhffm(input)),"", paste(info_colffm(input)),"", paste(info_munchffm(input))))
  clstation <- data.frame(lat = c(52.520007, 50.935173, 53.551086, 50.935173, 50.110922, 50.935173, 48.1351253, 50.935173),
                           long = c(13.404954, 6.953101, 9.993682, 6.953101, 8.682127, 6.953101, 11.5819806, 6.953101),
                           station = c("Berlin", "Berlin","Hamburg","Hamburg","Frankfurt","Frankfurt","Munich","Munich"),
                           info = c ("", paste(info_bercol(input)),"", paste(info_hhcol(input)),"", paste(info_colffm(input)),"", paste(info_munchcol(input))))
  mustation <- data.frame(lat = c(52.520007, 48.1351253, 53.551086, 48.1351253, 50.110922, 48.1351253, 50.935173, 48.1351253),
                          long = c(13.404954, 11.5819806, 9.993682, 11.5819806, 8.682127, 11.5819806, 6.953101, 11.5819806),
                          station = c("Berlin", "Berlin","Hamburg","Hamburg","Frankfurt","Frankfurt","Cologne","Cologne"),
                          info = c ("", paste(info_bermunch(input)),"", paste(info_hhmunch(input)),"", paste(info_munchffm(input)),"", paste(info_munchcol(input))))

  output$map <- renderLeaflet({

  

    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
    #  addTiles() %>%
      
  #group 6 Train route connections      
      addPolylines(
         data = berstation,
         lng = ~long[which(berstation$station == location())], 
         lat = ~lat[which(berstation$station == location())],
         weight = 2,
         opacity = 2,
         group = "DBOpenData",
        popup =  berstation$info[which(berstation$station == location())])%>%
      addPolylines(
        data = hhstation,
        lng = ~long[which(hhstation$station == location())], 
        lat = ~lat[which(hhstation$station == location())],
        weight = 2,
        opacity = 2,
        group = "DBOpenData",
        popup =  hhstation$info[which(hhstation$station == location())])%>%
      addPolylines(
        data = ffmstation,
        lng = ~long[which(ffmstation$station == location())], 
        lat = ~lat[which(ffmstation$station == location())],
        weight = 2,
        opacity = 2,
        group = "DBOpenData",
        popup =  ffmstation$info[which(ffmstation$station == location())])%>%
      addPolylines(
        data = clstation,
        lng = ~long[which(clstation$station == location())], 
        lat = ~lat[which(clstation$station == location())],
        weight = 2,
        opacity = 2,
        group = "DBOpenData",
        popup =  clstation$info[which(clstation$station == location())])%>%
      addPolylines(
        data = mustation,
        lng = ~long[which(mustation$station == location())], 
        lat = ~lat[which(mustation$station == location())],
        weight = 2,
        opacity = 2,
        group = "DBOpenData",
        popup =  mustation$info[which(mustation$station == location())])%>%
      
    #group 6 train starions  
      addMarkers(lng = coords[which(coords$loc == "Berlin"),]$lng,
                 lat = coords[which(coords$loc == "Berlin"),]$lat,
                  popup = paste(berlin_trainstation(input)),
                  icon = makeIcon("www\\Zug_icon.png", 30 , 30),
                 group ="DBOpenData")%>%
      addMarkers(lng = coords[which(coords$loc == "Munich"),]$lng,
                 lat = coords[which(coords$loc == "Munich"),]$lat,
                 popup = paste(munich_trainstation(input)),
                 icon = makeIcon("www\\Zug_icon.png", 30 , 30),
                 group ="DBOpenData")%>%
      addMarkers(lng = coords[which(coords$loc == "Frankfurt"),]$lng,
                 lat = coords[which(coords$loc == "Frankfurt"),]$lat,
                 popup = paste(frankfurt_trainstation(input)),
                 icon = makeIcon("www\\Zug_icon.png", 30 , 30),
                 group ="DBOpenData")%>%
      addMarkers(lng = coords[which(coords$loc == "Cologne"),]$lng,
                 lat = coords[which(coords$loc == "Cologne"),]$lat,
                 popup = paste(cologne_trainstation(input)),
                 icon = makeIcon("www\\Zug_icon.png", 30 , 30),
                 group ="DBOpenData")%>%
      addMarkers(lng = coords[which(coords$loc == "Hamburg"),]$lng,
                 lat = coords[which(coords$loc == "Hamburg"),]$lat,
                 popup = paste(hamburg_trainstation(input)),
                 icon = makeIcon("www\\Zug_icon.png", 30 , 30),
                 group ="DBOpenData")%>%
      
      
      addMarkers(lng = flinksterMarkerDF[which(flinksterMarkerDF$loc == input$location),]$lng,
                 lat = flinksterMarkerDF[which(flinksterMarkerDF$loc == input$location),]$lat, 
                 popup = flinksterMarkerDF[which(flinksterMarkerDF$loc == input$location),]$NAME,
                 group = "Flinkster rental stations",
                 icon = makeIcon("www\\flinksterIcon.png", 40, 40),
                 clusterOptions = markerClusterOptions()) %>%
      addMarkers(
        lng = osm(input)$lon,
        lat = osm(input)$lat, 
        popup = paste(osm(input)$amenity2),
        group = "OpenStreetMap",
        clusterOptions = markerClusterOptions()
      )%>%
      
    #group3 - OCM
    addMarkers(~Longitude, ~Latitude,
               data = df.g3(),
               clusterOptions = markerClusterOptions(),
               group = "OpenChargeMap",
               icon = ~EVIcons[as.factor(color)],   #as.factor(aux)      
               popup =~paste0(LocationTitle,"<br>", AddressLine1,"<br>",
                              AddressLine2,"<br>", Postcode,",",Town,"<br> ",
                              UsageType,sep = " ") ) %>% 
      addLegend (
        position = "bottomright",
        colors = c(unique(df.g3()$color)) ,
        labels =c(unique(df.g3()[which(df.g3()$color %in% c(unique(df.g3()$color))),]$UsageType)),
        opacity = 1,
        title = "EV charging stations: UsageType") %>%
        
        addAwesomeMarkers( data = g4cluster(input), 
                  lat = g4cluster(input)$lat,
                  lng = g4cluster(input)$lng,
                  popup = g4cluster(input)$text,
                  clusterOptions = markerClusterOptions(
                    iconCreateFunction=JS("function (cluster) {    
                      var childCount = cluster.getChildCount();
                      c = 'rgba(29, 202, 255, 1.0);'
                      return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span><b>' + 'T' + '</b></span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });                       }")),
                  clusterId = g4cluster(input)$city,
                  group = "Twitter",
                  icon = awesomeIcons(icon = "glyphicon-user", library="glyphicon")) %>% 
      
      addMarkers( clusterOptions = markerClusterOptions(),
                  lng = sensormarkers(input)$lon,
                  lat = sensormarkers(input)$lat,  
                  popup = paste("Pollution sensor ID:",sensormarkers(input)$sensor_id),
                  icon = makeIcon(iconUrl = "../2018-ws-mobility-dashboard/www/Luftdaten_icon.png"),
                  group = "luftdaten.info") %>%
  
      addCircles(lng = coords[which(coords$loc == input$location),]$lng,
      lat = coords[which(coords$loc == input$location),]$lat,
      radius=30000,
      fillColor = circlecolor(input),
      color=circlecolor(input),
      group = "luftdaten.info")%>%
      addLegend("bottomright", colors = c("#A75C00", "#D97700","#FF8C00","#FFBf70","#FFd6A4","#00436A","#00578A","#0EA5FF","#78CDFF","#A9DFFF"),
                labels = c("45 and higher", "40 until 45", "35 until 40","30 until 35", "25 until 30","20 until 25", "15 until 20", "10 until 15", "5 until 10", "under 5"),
                title="30km circle around city:<br>Air pollution in 25&microg/m&sup3",
                group = "luftdaten.info")%>%
      
      addLayersControl(
        position = "bottomleft",
        overlayGroups = groups,
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #Group 4: Adding and removing minicharts from the map according to a switch
  observeEvent(input$switch, {if(input$switch == F) {
    leafletProxy("map")%>% clearMinicharts() %>% removeControl("Twitter")
  } else {
    leafletProxy("map") %>%
      addMinicharts(
        g4miniChart(input)$lng,
        g4miniChart(input)$lat,
        type = "pie",
        chartdata = g4miniChart(input)[, c("Traffic", "Train")],
        colorPalette = c("#FF8C00", "#00436A"),
        width = 100,
        height = 100,
        transitionTime = 0)%>%
      removeControl("minichartsLegend") %>%
      addLegend (
        position = "topright",
        colors =c("#FF8C00", "#00436A"),
        labels =c("Traffic","Train"),
        opacity = 1,
        title = "Twitter minichart - Tweet freq.",
        layerId = "Twitter")
  }
    observeEvent(input$sidebar, {
      if(input$sidebar == "map"){
        shinyjs::show("switch")
      } else{
        shinyjs::hide("switch")
      }
    })
    
    
    
  })
  
}

shinyApp(ui = ui, server = server)