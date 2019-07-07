library(plotly)

#Suppress warnings
options(warn=-1)

#Load pre-calculated results
load("../2018-ws-mobility-dashboard/datafiles/Flinkster/results_Flinkster.RData")


#predefined color from VC course
flinkster_colors <- c('#FF8C00', '#2333FF', '#0EA5FF', '#FFBB00')

# Module server function
geoData1 <- function(geoData1, output, session, global) {
  
  #write code here
  observe({
    #get values from the gui
    filterCity <- global$location
    filterTime <- global$period
    
    #Rename timespan name to exlcude minus symbol
    if(filterTime=="three-days"){
      filterTime <- "threeDays"
    }
    dateIntervalBegin <- as.Date.character(eval(parse(text=paste0(filterTime,"[1]"))),c("%Y-%m-%d"))
    dateIntervalEnd <- as.Date.character(eval(parse(text=paste0(filterTime,"[2]"))),c("%Y-%m-%d"))
   
    as.Date(eval(parse(text=paste0(filterTime,"[1]"))), "%Y/%m/%d", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
     #for local tests
    #filterCity <- "Berlin"
    #filterTime <- "Yearly"
  
    #Select suitable resultdf for selected city and timespan 
    resultDF <- as.data.frame(eval(parse(text=paste0("resultDF_",filterCity))))
    resultDF <- resultDF[resultDF$TIMESPAN==filterTime,]
    
    #Select suitable resultdf_bar_pie for selected city
    resultDF_bar_pie <- as.data.frame(eval(parse(text=paste0("resultDF_bar_pie_",filterCity))))
  
    desired_maximum_marker_size <- 50
    sizeref <- 2.0 * max(resultDF$NBR_OF_BOOKINGS) / (desired_maximum_marker_size**2)
  
    # Error in plotly package causing warning
    # https://github.com/ropensci/plotly/issues/1367
    # Scaling issue Bubblesize in legend
    # https://github.com/plotly/plotly.js/issues/2080
    
    #Bubblechart
    output$diagram1 <- renderPlotly({
      plot_ly(as.data.frame(resultDF),
              x = ~MEDIAN_KM_VEHICLE_GROUP,
              y = ~VEHICLE_KW_GROUP,
              mode = 'markers',
              hoverinfo = "text",
              hovertext = ~paste('Number of bookings:',NBR_OF_BOOKINGS,'<br>City:',filterCity),
              type = 'scatter',
              size = ~NBR_OF_BOOKINGS,
              color = ~FUEL_TYPE_NAME,
              colors = flinkster_colors, 
              sizes = c(min(resultDF$NBR_OF_BOOKINGS),max(resultDF$NBR_OF_BOOKINGS)),
              marker = list(opacity = 0.7,sizeref = sizeref)) %>%
        layout(title = 'Number of bookings by distance,\nmotorization and fuel type<br />',
               margin = list(l = 10,r = 10,b = 20,t = 70,pad = 4),
               autosize = T, margin = list(l = 10,r = 10,b = 20,t = 50,pad = 4),
               annotations = list(x = resultDF[which.max(resultDF$NBR_OF_BOOKINGS), ]$MEDIAN_KM_VEHICLE_GROUP,
                                  y = resultDF[which.max(resultDF$NBR_OF_BOOKINGS), ]$VEHICLE_KW_GROUP,
                                  text = resultDF[which.max(resultDF$NBR_OF_BOOKINGS), ]$NBR_OF_BOOKINGS,
                                  xref = "x",yref = "y",
                                  showarrow = FALSE,
                                  xanchor = "center",yanchor = "middle"),
               xaxis = list(title = 'Median distance driven per trip',showgrid = TRUE),
               yaxis = list(title = 'Power distribution in KW',showgrid = TRUE),
               showlegend = TRUE) %>%
        add_annotations(x = 1.4,
                        y = 0.4,
                        text = ~paste('<b>Date interval:</b>\n', format(dateIntervalBegin,"%Y %b %d"), '\n-\n', format(dateIntervalEnd,"%Y %b %d"), '\n\n\n<b>No. bookings:</b>\n', sum(resultDF$NBR_OF_BOOKINGS)),
                        xref = "paper",
                        yref = "paper",
                        showarrow = FALSE) %>%
        config(displayModeBar = F)
    })
      
    # Barchart
    # issue found on text alignment when using the hover info
    # if the hover info is aligned on the left site, text appears aligned right and vice versa
    # for more information see: https://github.com/plotly/plotly.js/issues/260
      
    output$diagram2 <- renderPlotly({
      subplot(
        plot_ly(
          y=c(resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Diesel"], 
              resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Electricity"],
              resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Gasoline"],
              resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="PlugIn Hybrid"]),
          #  x=resultDF_bar_pie$FUEL_TYPE_NAME,
          x=c("Diesel","Electricity","Gasoline","PlugIn Hybrid"),
          type = 'bar',
          name = "Bar",
          color = c("Diesel","Electricity","Gasoline","PlugIn Hybrid"),
          colors = flinkster_colors, 
          legendgroup = "a",
          showlegend = FALSE,
          hoverinfo = 'text',
          text = ~paste('Median distance: ', c(resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Diesel"], 
                                               resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Electricity"],
                                               resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="Gasoline"],
                                               resultDF_bar_pie$MEDIAN_KM_VEHICLE_GROUP[resultDF_bar_pie$FUEL_TYPE_NAME=="PlugIn Hybrid"]),'<br>Fueltype: ',c("Diesel","Electricity","Gasoline","PlugIn Hybrid"))) %>%
          layout(shapes = list(x0 = 0,
                               x1 = 1, 
                               xref = "paper",
                               y0 = max(as.numeric(resultDF_bar_pie$OVERALL_MEDIAN)), 
                               y1 = max(as.numeric(resultDF_bar_pie$OVERALL_MEDIAN)),
                               line = list(color = "grey")
          ), annotations = list(
            xref="paper",
            xanchor=1.1,
            y = as.numeric(max(resultDF_bar_pie$OVERALL_MEDIAN)),#+1.5,
            text = as.numeric(max(resultDF_bar_pie$OVERALL_MEDIAN)),
            showarrow = FALSE
          )
          ) %>%
          config(displayModeBar = F), 
        plot_ly(as.data.frame(resultDF_bar_pie),
                labels = c("Diesel","Electricity","Gasoline","PlugIn Hybrid"), 
                values = c(resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Diesel"], 
                           resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Electricity"],
                           resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Gasoline"],
                           resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="PlugIn Hybrid"]), 
                type = 'pie',
                name = "Pie",
                legendgroup = "a",
                domain = list(x = c(0.65, 0.9), y = c(0.5, 0.9)),
                hole = 0.5,
                rotation = 180,
                direction = "clockwise",
                textposition = 'outside',
                textinfo = 'percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('Number of Bookings:',format(as.numeric(c(resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Diesel"], 
                                                                        resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Electricity"],
                                                                        resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="Gasoline"],
                                                                        resultDF_bar_pie$NUMBER_OF_BOOKINGS[resultDF_bar_pie$FUEL_TYPE_NAME=="PlugIn Hybrid"])),nsmall=0, big.mark=" "),'<br>Fueltype:',c("Diesel","Electricity","Gasoline","PlugIn Hybrid")),
                marker = list(colors = flinkster_colors,
                              line = list(color = '#FFFFFF', width = 1))) %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          config(displayModeBar = F),
        shareX = FALSE,
        shareY =FALSE) %>%
        add_annotations(x = 0.9,
                        y = 0.0025,
                        text = ~paste('<b>No. bookings:</b>\n', sum(resultDF_bar_pie$NUMBER_OF_BOOKINGS)),
                        xref = "paper",
                        yref = "paper",
                        showarrow = FALSE) %>%
        layout(title = 'Bar chart: Avgerage driven distances by fuel type\nPie chart: Fuel type distribution by trip bookings\nPeriod: 2013 - 2017',
               margin = list(l = 10,r = 10,b = 20,t = 70,pad = 4),
               yaxis = list(title= 'Median of driven distances in km'),
               xaxis = list(title= 'Fuel type',showticklabels = FALSE),
               showlegend = TRUE,
               legend = list(orientation = "h",
                             x = 0.65, y = 0.4,
                             font = list(size = 12)))
    })
  })
#end code here
}
