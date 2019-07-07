# Module server function
geoData3 <- function(input, output, session, global) {

# --- read CSV file
  
  #--- load new csv file
    #ocm_file <- "C:/Users/Swob1/OneDrive/Desktop/2018-ws-mobility-dashboard/Dashboard/datafiles/database_group3.csv"
    #download.file("https://api.openchargemap.io/v2/poi/?output=csv&countrycode=DE&maxresults=15000", ocm_file)
    #OCMData <- read.csv(ocm_file)
  
    #--- recalculate distance matrix for new dataset and save the csv file as "database_group3_avg_distances"
    #source("../2018-ws-mobility-dashboard/functions/g3_avg_distance_matrix.R")
    
  
  #--- open local csv file
    #--- Option 1
    #OCMData <- read.csv("~path") # path of csv-file
    
    #--- Option 2
    #OCMData <- read.csv(file.choose(), header=TRUE)
    
    #---option 3
    OCMData <- read.csv(file = "datafiles/OpenChargeMap/database_group3.csv" , header = TRUE) 
    

  # ---explore and fix data ---
  #summary(OCMData)
  unique(OCMData$UsageType)
  colnames(OCMData)[1] <- "ID"
  OCMData$Postcode <- as.numeric(as.character(OCMData$Postcode))
  
  # --- colors
  OCMcolors <- c('rgb(14,165,255)', 'rgb(255,140,0)', 'rgb(35,51,255)', 'rgb(255,187,0)', 'rgb(141,206,245)')

  # create column to assign color to each usagetype  
  OCMData$color <- "gray"
  OCMData[which(OCMData$UsageType %in% "Public"),]$color <- "blue" #blue
  OCMData[which(OCMData$UsageType %in% "Public - Membership Required"),]$color <- "dodgerblue" 
  OCMData[which(OCMData$UsageType %in% "Public - Pay At Location"),]$color <- "darkturquoise"
  OCMData[which(OCMData$UsageType %in% "Public - Notice Required"),]$color <- 'lightblue' #light
  OCMData[which(OCMData$UsageType %in% "Private - For Staff, Visitors or Customers"),]$color <- "darkorange"
  OCMData[which(OCMData$UsageType %in% "Private - Restricted Access"),]$color <- "orange"
  OCMData[which(OCMData$UsageType %in% "Privately Owned - Notice Required"),]$color <- "gold"
  

  # ---variable declaration ---
  vector_cities <- c("Berlin","Hamburg","Frankfurt","Cologne","Munich")
  
 
  #--------
  # Function to get postcode
  #--------
  postcode <- function(selected_city){
    
    pc <- ifelse(selected_city %in% vector_cities[1],10000, #berlin
                 ifelse(selected_city %in% vector_cities[2],20000, #hamburg
                        ifelse(selected_city %in% vector_cities[3],60000, #frankfurt
                               ifelse(selected_city %in% vector_cities[4],50000, #cologne
                                      ifelse(selected_city %in% vector_cities[5],80000) #munich
                               )
                        )
                 )
    )
    return(pc)
    
  }
  
    # --- use selected city
    selected_city <- reactive({global$location}) #!!this can be directly used
  
    
    # function to subset data set to selected city
    df.selected.city <- reactive({
      # --- use selected city
      
      #validate variation of name of the cities
      ifelse(selected_city() %in% "Munich",
             df.city <- OCMData[grepl('Muenchen|Munich|Munchen|München|MÃ¼nchen',OCMData$StateOrProvince) | grepl('Muenchen|Munich|Munchen|München|MÃ¼nchen',OCMData$Town),],
             ifelse(selected_city() %in% "Cologne",
                    df.city <- OCMData[grepl('KÃ¶ln|Cologne|Keulen|Koeln|Köln',OCMData$StateOrProvince) | grepl('KÃ¶ln|Cologne|Keulen|Koeln|Köln',OCMData$Town),],
                    df.city <- OCMData[grepl(selected_city(),OCMData$StateOrProvince) | grepl(selected_city(),OCMData$Town),]))
      
      #validate postcode
      df.city <- df.city[which(df.city$Postcode < postcode(selected_city())+9999 & df.city$Postcode >= postcode(selected_city())),]
      
      #unifies unknown, empty values to "unknown"
      df.city$UsageType <- ifelse(df.city$UsageType == "(Unknown)" | df.city$UsageType == "" | is.na(df.city$UsageType), "Unknown", as.character(df.city$UsageType))
      
      
      return(df.city)
    })
    
    
    # --- output graphs ---
    
    
    
    # pie chart = total locations + type of charging station distribution
    output$PieChart <- renderPlotly({
      
      Pie <- df.selected.city() %>%
        group_by(UsageType,color) %>%
        summarize(Total = n()) %>%
        filter(Total > 0) %>%
        {. ->> Ttl} %>%
        plot_ly(labels = ~UsageType, values = ~Total, name = "",
                textfont = list(color = '#FFFFF', size = 16), 
                textinfo = 'value',
                marker = list(opacity = '0', colors = OCMcolors,line = list(color = '#FFFFFF', width = 1)),
                sort=T,
                x = 0,
                y = 0) %>%
                add_pie(hole = 0.6) %>%
                add_text(text = ~paste("<b>EV Charging</b>", "<b>Stations:</b>", sum(Ttl[3]), sep = "<br>"), name = selected_city(), x = 5, y = 10, showlegend = F) %>%
                layout(showlegend = T,
                       legend = list(orientation = "topright", font = list(size = 10)),
                      font=list(family="calibri", size = 18),
                      plot_bgcolor="#FFFFF", textinfo='text',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      title = paste("Distribution of EV charging stations in", selected_city(), "<br>" ,sep = " "),
                      titlefont=list(size=20)) %>%
        config(displayModeBar = F) %>%
        config(showLink = F) 
      
      Pie
    })
    

  output$avg_distance_chart <- renderPlotly({
    Average_Distances <- read.csv(file = "datafiles/OpenChargeMap/database_group3_avg_distances.csv" , header = TRUE) 
    
    Avg_Distance_Chart <- Average_Distances %>%
      plot_ly(x = ~vector_cities, 
              y = ~round(vector_average_distance, digits=2),
              type = 'bar', 
              name = 'Total',
              marker = list(color = OCMcolors[1])) %>%
              add_trace(y = ~round(vector_public_distance, digits=2), name = 'Public', marker = list(color = OCMcolors[2])) %>%
              add_trace(y = ~round(vector_private_distance, digits=2), name = 'Private', marker = list(color = OCMcolors[3])) %>%
              add_trace(y = ~round(vector_unknown_distance, digits=2), name = 'Unknown', marker = list(color = OCMcolors[4])) %>%
              layout(title = 'Average Distances between EV charging stations',
                    xaxis = list(
                    title = "",
                    tickfont = list(
                    size = 14,
                    color = 'rgb(107, 107, 107)')),
                    yaxis = list(
                    title = 'Kilometers',
                    titlefont = list(
                    size = 16,
                    color = 'rgb(107, 107, 107)'),
                    tickfont = list(
                    size = 14,
                    color = 'rgb(107, 107, 107)')),
                    legend = list(bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                    barmode = 'group', bargap = 0.15, bargroupgap = 0.1) %>%
      config(displayModeBar = F) %>%
      config(showLink = F)
    
    Avg_Distance_Chart
    
  })
  
  return(reactive({df.selected.city()}))
  #end code here

}

EVIcons <- iconList(
  #"gray","blue", "dodgerblue" "darkturquoise"'lightblue' "darkorange" "orange" "gold"
  gray = makeIcon("www\\OCM_icons\\EV-Charging - gray.png",iconWidth = 40, iconHeight = 40),
  blue = makeIcon("www\\OCM_icons\\EV-Charging - blue.png",iconWidth = 40, iconHeight = 40),
  dodgerblue = makeIcon("www\\OCM_icons\\EV-Charging - dodgerblue.png",iconWidth = 40, iconHeight = 40),
  darkturquoise = makeIcon("www\\OCM_icons\\EV-Charging - darkturquoise.png",iconWidth = 40, iconHeight = 40),
  lightblue = makeIcon("www\\OCM_icons\\EV-Charging - lightblue.png",iconWidth = 40, iconHeight = 40),
  darkorange = makeIcon("www\\OCM_icons\\EV-Charging - darkorange.png",iconWidth = 40, iconHeight = 40),
  orange = makeIcon("www\\OCM_icons\\EV-Charging - orange.png",iconWidth = 40, iconHeight = 40),
  gold = makeIcon("www\\OCM_icons\\EV-Charging - gold.png",iconWidth = 40, iconHeight = 40)
  
  
  
) 
