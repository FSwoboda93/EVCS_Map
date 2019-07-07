# Module server function
library(ggplot2)
library(readr)


geoData6 <- function(geoData6, output, session, global) {
  
  
  filename <- reactive ({
    paste("plotData6/",global$location,".csv",sep="")
  })
  
  data <- reactive ({
    read_csv2(filename())
    #data2$meanDown <- as.numeric(as.character(data2$meanDown))
    #data2$meanUp <- as.numeric(as.character(data2$meanUp))
    #data2$mean <- ifelse(is.na(data2$meanDown), data2$meanUp, data2$meanDown)
  })
  
  output$diagram6 <- renderPlot({
        ggplot(data(), x=group, y=mean, aes(routes, mean, fill = rates))+  
        geom_bar(stat="identity", position=position_dodge())+
        geom_hline(aes(yintercept = mean(meanDown, na.rm = TRUE), linetype = "Uploadrate"), data(),color="#2333FF")+
        geom_hline(aes(yintercept = mean(meanUp, na.rm = TRUE),  linetype = "Downloadrate") , data(),color="#FF8C00")+
        scale_fill_manual(name = "AverageRates on routes in Kbit/s", values=c("#2333FF", "#FF8C00", "#66CC99"))+
        scale_linetype_manual(name = "AveragesRates over all routes in Kbit/s", values = c(2, 2), guide = guide_legend(override.aes = list(color = c("#2333FF", "#FF8C00"))))+
        xlab("Routes") +
        ylab("Mean")+
        ggtitle("Average Upload/Downloadrates for important routes from the selected city")
    })
       
        
        #unused..tried infobox on hover only with ggplot, doesnt really work. The Problem could be that the hover infobox and the output table must be in the same div
        #in order to show up correctly + the reactive data could also cause the problems, see (https://gitlab.com/snippets/16220)
        output$hover_info <- renderUI({
          hover <- input$plot_hover
          point <- nearPoints(ourData, hover, addDist = TRUE)
          if (nrow(point) == 0) return(NULL)
          
          # calculate point position INSIDE the image as percent of total dimensions
          # from left (horizontal) and from top (vertical)
          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
          
          # calculate distance from left and bottom side of the picture in pixels
          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
          
          # create style property fot tooltip
          # background color is set so tooltip is a bit transparent
          # z-index is set so we are sure are tooltip will be on top
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_px + 2, "px; top:", top_px + 2, "px;")
          
          # actual tooltip created as wellPanel
          wellPanel(
            style = style,
            p(HTML(paste0("<b> Route: </b>",rownames(point$routes), "<br/>",
                          "<b> ist </b>",rownames(point$mean), "<br/>",
                          "<b> ein test</b>", "<br/>",
                          "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
          )
        })

      
        
        
        
   
}