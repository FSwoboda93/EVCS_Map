# Module server function
library(ggplot2)
library(lubridate)
#library(plyr)
library(dplyr)
library(scales)

source("../2018-ws-mobility-dashboard/functions/avg_data.R")



geoData5 <- function(input, output, session, global) {
  
##mockdata for 2 Sprint----
  
  # day8761<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/day8761.csv")
  # day8761$timestamp <- ymd_hms(day8761$timestamp)
  # day8761 <- create_avg_data(day8761)
  # day8761$limit=25
  # 
  # week8761<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/week8761.csv")
  # week8761$timestamp <- ymd_hms(week8761$timestamp)
  # week8761<-create_avg_data(week8761)
  # week8761$limit=25
  # 
  # day13030 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/day13030.csv")
  # day13030$timestamp <- ymd_hms(day13030$timestamp)
  # day13030 <- create_avg_data(day13030)
  # day13030$limit=25
  # 
  # week13030 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/week13030.csv")
  # week13030$timestamp <- ymd_hms(week13030$timestamp)
  # week13030<-create_avg_data(week13030)
  # week13030$limit=25
  # 
  # day16397 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/day16397.csv")
  # day16397$timestamp <- ymd_hms(day16397$timestamp)
  # day16397 <- create_avg_data(day16397)
  # day16397$limit=25
  # 
  # week16397 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/week16397.csv")
  # week16397$timestamp <- ymd_hms(week16397$timestamp)
  # week16397 <- create_avg_data(week16397,6)
  # week16397$limit=25
  # 
  # day16632 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/day16632.csv")
  # day16632$timestamp <- ymd_hms(day16632$timestamp)
  # day16632 <- create_avg_data(day16632)
  # day16632$limit=25
  # 
  # week16632 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/week16632.csv")
  # week16632$timestamp <- ymd_hms(week16632$timestamp)
  # week16632 <- create_avg_data(week16632,9)
  # week16632$limit=25
  # 
  # 
  # #data of sensor 14880 was reduced in data preparation file already
  # day14880 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/day14880.csv")
  # day14880$avg_date <- ymd_hms(day14880$avg_date)
  # day14880$limit=25
  # day14880 <- dplyr::select(day14880, avg_date, avg_p1, limit)
  # #day14880
  # week14880 <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/mock/week14880.csv")
  # week14880$avg_date <- ymd_hms(week14880$avg_date)
  # week14880$limit=25
  # week14880 <- dplyr::select(week14880, avg_date, avg_p1, limit)
  # 
  # #summary(week14880)
  # #summary(day14880)
  # #summary(week16632)
  # #summary(day16632)
  
##data prep----
  ###avg----
  avgdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cities_days.csv")
  avgdays <- dplyr::select(avgdays, timestamp, P2)
  avgdays <- na.omit(avgdays)
  avgdays <- plyr::rename(avgdays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  avgdays$avg_date <- ymd_hms(avgdays$avg_date)
  avgdays$avg_p1 <- as.numeric(as.character(avgdays$avg_p1))
  avgweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cities_week.csv")
  avgweek <- dplyr::select(avgweek, timestamp, P2)
  avgweek <- na.omit(avgweek)
  avgweek <- plyr::rename(avgweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  avgweek$avg_date <- ymd_hms(avgweek$avg_date)
  avgweek$avg_p1 <- as.numeric(as.character(avgweek$avg_p1))
  avgmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cities_month.csv")
  avgmonth <- dplyr::select(avgmonth, timestamp, P2)
  avgmonth <- na.omit(avgmonth)
  avgmonth <- plyr::rename(avgmonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  avgmonth$avg_date <- ymd_hms(avgmonth$avg_date)
  avgmonth$avg_p1 <- as.numeric(as.character(avgmonth$avg_p1))
  avgyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cities_year.csv")
  avgyear <- dplyr::select(avgyear, timestamp, P2)
  avgyear <- na.omit(avgyear)
  avgyear <- plyr::rename(avgyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  avgyear$avg_date <- ymd_hms(paste(avgyear$avg_date," 12:00:00"))
  avgyear$avg_p1 <- as.numeric(as.character(avgyear$avg_p1))
  
  
  ###data berlin----
  berlindays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_days.csv")
  berlindays <- dplyr::select(berlindays, timestamp, P2)
  berlindays <- na.omit(berlindays)
  berlindays <- plyr::rename(berlindays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  berlindays$avg_date <- ymd_hms(berlindays$avg_date)
  berlindays$avg_p1 <- as.numeric(as.character(berlindays$avg_p1))
  berlinweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_week.csv")
  berlinweek <- dplyr::select(berlinweek, timestamp, P2)
  berlinweek <- na.omit(berlinweek)
  berlinweek <- plyr::rename(berlinweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  berlinweek$avg_date <- ymd_hms(berlinweek$avg_date)
  berlinweek$avg_p1 <- as.numeric(as.character(berlinweek$avg_p1))
  berlinmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_month.csv")
  berlinmonth <- dplyr::select(berlinmonth, timestamp, P2)
  berlinmonth <- na.omit(berlinmonth)
  berlinmonth <- plyr::rename(berlinmonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  berlinmonth$avg_date <- ymd_hms(berlinmonth$avg_date)
  berlinmonth$avg_p1 <- as.numeric(as.character(berlinmonth$avg_p1))
  berlinyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_year.csv")
  berlinyear <- dplyr::select(berlinyear, timestamp, P2)
  berlinyear <- na.omit(berlinyear)
  berlinyear <- plyr::rename(berlinyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  berlinyear$avg_date <- ymd_hms(paste(berlinyear$avg_date," 12:00:00"))
  berlinyear$avg_p1 <- as.numeric(as.character(berlinyear$avg_p1))
  
  
  ###data cologne----
  colognedays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_days.csv")
  colognedays <- dplyr::select(colognedays, timestamp, P2)
  colognedays <- na.omit(colognedays)
  colognedays <- plyr::rename(colognedays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  colognedays$avg_date <- ymd_hms(colognedays$avg_date)
  colognedays$avg_p1 <- as.numeric(as.character(colognedays$avg_p1))
  cologneweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_week.csv")
  cologneweek <- dplyr::select(cologneweek, timestamp, P2)
  cologneweek <- na.omit(cologneweek)
  cologneweek <- plyr::rename(cologneweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  cologneweek$avg_date <- ymd_hms(cologneweek$avg_date)
  cologneweek$avg_p1 <- as.numeric(as.character(cologneweek$avg_p1))
  colognemonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_month.csv")
  colognemonth <- dplyr::select(colognemonth, timestamp, P2)
  colognemonth <- na.omit(colognemonth)
  colognemonth <- plyr::rename(colognemonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  colognemonth$avg_date <- ymd_hms(colognemonth$avg_date)
  colognemonth$avg_p1 <- as.numeric(as.character(colognemonth$avg_p1))
  cologneyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_year.csv")
  cologneyear <- dplyr::select(cologneyear, timestamp, P2)
  cologneyear <- na.omit(cologneyear)
  cologneyear <- plyr::rename(cologneyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  cologneyear$avg_date <- ymd_hms(paste(cologneyear$avg_date," 12:00:00"))
  cologneyear$avg_p1 <- as.numeric(as.character(cologneyear$avg_p1))
  
  ###data frankfurt----
  frankfurtdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_days.csv")
  frankfurtdays <- dplyr::select(frankfurtdays, timestamp, P2)
  frankfurtdays <- na.omit(frankfurtdays)
  frankfurtdays <- plyr::rename(frankfurtdays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  frankfurtdays$avg_date <- ymd_hms(frankfurtdays$avg_date)
  frankfurtdays$avg_p1 <- as.numeric(as.character(frankfurtdays$avg_p1))
  frankfurtweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_week.csv")
  frankfurtweek <- dplyr::select(frankfurtweek, timestamp, P2)
  frankfurtweek <- na.omit(frankfurtweek)
  frankfurtweek <- plyr::rename(frankfurtweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  frankfurtweek$avg_date <- ymd_hms(frankfurtweek$avg_date)
  frankfurtweek$avg_p1 <- as.numeric(as.character(frankfurtweek$avg_p1))
  frankfurtmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_month.csv")
  frankfurtmonth <- dplyr::select(frankfurtmonth, timestamp, P2)
  frankfurtmonth <- na.omit(frankfurtmonth)
  frankfurtmonth <- plyr::rename(frankfurtmonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  frankfurtmonth$avg_date <- ymd_hms(frankfurtmonth$avg_date)
  frankfurtmonth$avg_p1 <- as.numeric(as.character(frankfurtmonth$avg_p1))
  frankfurtyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_year.csv")
  frankfurtyear <- dplyr::select(frankfurtyear, timestamp, P2)
  frankfurtyear <- na.omit(frankfurtyear)
  frankfurtyear <- plyr::rename(frankfurtyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  frankfurtyear$avg_date <- ymd_hms(paste(frankfurtyear$avg_date," 12:00:00"))
  frankfurtyear$avg_p1 <- as.numeric(as.character(frankfurtyear$avg_p1))
  
  ###data hamburg----
  hamburgdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_days.csv")
  hamburgdays <- dplyr::select(hamburgdays, timestamp, P2)
  hamburgdays <- na.omit(hamburgdays)
  hamburgdays <- plyr::rename(hamburgdays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  hamburgdays$avg_date <- ymd_hms(hamburgdays$avg_date)
  hamburgdays$avg_p1 <- as.numeric(as.character(hamburgdays$avg_p1))
  hamburgweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_week.csv")
  hamburgweek <- dplyr::select(hamburgweek, timestamp, P2)
  hamburgweek <- na.omit(hamburgweek)
  hamburgweek <- plyr::rename(hamburgweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  hamburgweek$avg_date <- ymd_hms(hamburgweek$avg_date)
  hamburgweek$avg_p1 <- as.numeric(as.character(hamburgweek$avg_p1))
  hamburgmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_month.csv")
  hamburgmonth <- dplyr::select(hamburgmonth, timestamp, P2)
  hamburgmonth <- na.omit(hamburgmonth)
  hamburgmonth <- plyr::rename(hamburgmonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  hamburgmonth$avg_date <- ymd_hms(hamburgmonth$avg_date)
  hamburgmonth$avg_p1 <- as.numeric(as.character(hamburgmonth$avg_p1))
  hamburgyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_year.csv")
  hamburgyear <- dplyr::select(hamburgyear, timestamp, P2)
  hamburgyear <- na.omit(hamburgyear)
  hamburgyear <- plyr::rename(hamburgyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  hamburgyear$avg_date <- ymd_hms(paste(hamburgyear$avg_date," 12:00:00"))
  hamburgyear$avg_p1 <- as.numeric(as.character(hamburgyear$avg_p1))
  
  ###data munich----
  munichdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_days.csv")
  munichdays <- dplyr::select(munichdays, timestamp, P2)
  munichdays <- na.omit(munichdays)
  munichdays <- plyr::rename(munichdays, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  munichdays$avg_date <- ymd_hms(munichdays$avg_date)
  munichdays$avg_p1 <- as.numeric(as.character(munichdays$avg_p1))
  munichweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_week.csv")
  munichweek <- dplyr::select(munichweek, timestamp, P2)
  munichweek <- na.omit(munichweek)
  munichweek <- plyr::rename(munichweek, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  munichweek$avg_date <- ymd_hms(munichweek$avg_date)
  munichweek$avg_p1 <- as.numeric(as.character(munichweek$avg_p1))
  munichmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_month.csv")
  munichmonth <- dplyr::select(munichmonth, timestamp, P2)
  munichmonth <- na.omit(munichmonth)
  munichmonth <- plyr::rename(munichmonth, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  munichmonth$avg_date <- ymd_hms(munichmonth$avg_date)
  munichmonth$avg_p1 <- as.numeric(as.character(munichmonth$avg_p1))
  munichyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_year.csv")
  munichyear <- dplyr::select(munichyear, timestamp, P2)
  munichyear <- na.omit(munichyear)
  munichyear <- plyr::rename(munichyear, c("timestamp" = "avg_date", "P2" = "avg_p1"))
  munichyear$avg_date <- ymd_hms(paste(munichyear$avg_date," 12:00:00"))
  munichyear$avg_p1 <- as.numeric(as.character(munichyear$avg_p1))
  
  ###variables colours----
  colours <- c("#A9DFFF","#78CDFF","#0EA5FF","#00578A","#00436A")
  anticolours <- c("#FFd6A4","#FFBf70","#FF8C00","#D97700","#A75C00")
  
##plot output----
  output$diagram5 <-renderPlot({
    if(global$location == "Berlin"){
      if(global$period == "three-days"){
        plotdata <- berlindays
        average <- avgdays
      }
      if(global$period == "weekly"){
        plotdata <- berlinweek
        average <- avgweek
      }
      if(global$period == "monthly"){
        plotdata <- berlinmonth
        average <- avgmonth
      }
      if(global$period == "yearly"){
        plotdata <- berlinyear
        average <- avgyear
      }
    }
    
    if(global$location == "Hamburg"){
      if(global$period == "three-days"){
        plotdata <- hamburgdays
        average <- avgdays
      } 
      if(global$period == "weekly"){
        plotdata <- hamburgweek
        average <- avgweek
      }
      if(global$period == "monthly"){
        plotdata <- hamburgmonth
        average <- avgmonth
      }
      if(global$period == "yearly"){
        plotdata <- hamburgyear
        average <- avgyear
      }  
    }
    
    if(global$location == "Frankfurt"){
      if(global$period == "three-days"){
        plotdata <- frankfurtdays
        average <- avgdays
      } 
      if(global$period == "weekly"){
        plotdata <- frankfurtweek
        average <- avgweek
      }
      if(global$period == "monthly"){
        plotdata <- frankfurtmonth
        average <- avgmonth
      }
      if(global$period == "yearly"){
        plotdata <- frankfurtyear
        average <- avgyear
      }
    }
    
    if(global$location == "Cologne"){
      if(global$period == "three-days"){
        plotdata <- colognedays
        average <- avgdays
      } 
      if(global$period == "weekly"){
        plotdata <- cologneweek
        average <- avgweek
      }
      if(global$period == "monthly"){
        plotdata <- colognemonth
        average <- avgmonth
      }
      if(global$period == "yearly"){
        plotdata <- cologneyear
        average <- avgyear
      }
    }
    
    if(global$location == "Munich"){
      if(global$period == "three-days"){
        plotdata <- munichdays
        average <- avgdays
      }
      if(global$period == "weekly"){
        plotdata <- munichweek
        average <- avgweek
      }
      if(global$period == "monthly"){
        plotdata <- munichmonth
        average <- avgmonth
      }
      if(global$period == "yearly"){
        plotdata <- munichyear
        average <- avgyear
      }
    }
    
    plotdata$limit = 25
    
    #create temporary dataframe rowadd and boolean variable breach
    avg_date <- ymd_hms(c(), tz = "CET")
    avg_p1 <- c()
    rowadd <- data.frame(avg_date,avg_p1)
    breach <- TRUE
    
    #look for sections where limit is breached and save values before and after breach 
    for(i in 2:nrow(plotdata)){
      if(plotdata$avg_p1[i]>25 && breach==TRUE){
        rowadd <- rbind(rowadd,slice(plotdata,(i-1):i))
        breach <- FALSE
      }
      if(plotdata$avg_p1[i]<=25 && breach==FALSE){
        rowadd <- rbind(rowadd,slice(plotdata,(i-1):i))
        breach <-TRUE
      }
    }
    
    #find coordinates of breach points and add those to dataframe used for plotting
    if(nrow(rowadd)!=0){
      for(i in seq(2,nrow(rowadd),by=2)){
        rowadd$timeinterval[i] <- difftime(time1=rowadd$avg_date[i],time2=rowadd$avg_date[i-1],unit= "secs")
      }
      rowadd[is.na(rowadd)] <- 0
      
      for(i in seq(2,nrow(rowadd),by=2)){
        p1_slope <- rowadd$avg_p1[i]-rowadd$avg_p1[i-1]
        xfactor <- p1_slope/(rowadd$timeinterval[i])
        interrow <- data.frame(avg_date=rowadd$avg_date[i-1]+(25-rowadd$avg_p1[i-1])/xfactor,avg_p1=25,limit=25)
        plotdata <- rbind(plotdata, interrow)
      }
    }
    
    
    #sort dataframe for plotting by date
    plotdata <- plotdata[order(plotdata$avg_date),]
    
    #create individual subset with data over and equal limit 
    over_limit <- subset(plotdata,avg_p1>=25)
    max <- subset(plotdata,avg_p1==max(plotdata$avg_p1)) 
    
    
    ggplot(plotdata,aes(x=avg_date))+ 
      geom_ribbon(data=average,x=average$avg_date, ymin=0,ymax=average$avg_p1,fill=colours[1])+
      geom_ribbon(ymin=0,ymax=plotdata$avg_p1,fill=colours[4])+
      geom_ribbon(data=over_limit,aes(ymin=limit,ymax=avg_p1),colour=anticolours[2],fill=anticolours[2])+
      geom_line(data=average,aes(x=avg_date, y=avg_p1, colour="avg")) + 
      geom_line(aes(y=avg_p1,colour="pm"))+ 
      geom_line(data=over_limit,aes(y=avg_p1),colour=anticolours[3])+ 
      geom_line(aes(y=limit, colour="lim"), size=1)+ 
      geom_point(data=max,aes(x=max$avg_date,y=max$avg_p1, colour="max", size=5), shape=1)+
      geom_text(data=max,aes(x=max$avg_date,y=max$avg_p1,colour="max",
                             label=round(avg_p1,digits =1), size=5),
                show.legend=FALSE, vjust=0.1, hjust=-0.3)+
      theme_bw() +
      labs(x="Time", y=expression(paste("Particle matter in 25 ",mu,"g/","m"^3))) +
      scale_colour_manual(values= c("avg"= colours[3],"pm"= colours[4],"lim"= anticolours[4],
                                    "max"=anticolours[5]),
                          labels= c("avg"="Average pollution  ",
                                    "pm"= bquote("Pollution in"~ .(global$location) ) ,
                                    "lim"="Government limitation  ", "max"="Maximum value "))+
      scale_x_datetime(labels=date_format("%Y %b %d"))+
      guides(size="none",colour=guide_legend(title = NULL,nrow = 2, reverse = TRUE))+
      theme(title = element_text(size=14),legend.text = element_text(size=14),
            legend.position = "bottom", axis.text.x = element_text(size=13),
            axis.text.y = element_text(size=13))
    
  })
##textoutput----
  output$text5 <- renderText({
    if(global$location == "Berlin"){
      if(global$period == "three-days"){
        count <- nrow(filter(berlindays, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_days.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "weekly"){
        count <- nrow(filter(berlinweek, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_week.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "monthly"){
        count <- nrow(filter(berlinmonth, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_month.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "yearly"){
        count <- nrow(filter(berlinyear, avg_p1>25)) 
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_year.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
    }

    if(global$location == "Hamburg"){
      if(global$period == "three-days"){
        count <- nrow(filter(hamburgdays, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_days.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "weekly"){
        count <- nrow(filter(hamburgweek, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_week.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "monthly"){
        count <- nrow(filter(hamburgmonth, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_month.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "yearly"){
        count <- nrow(filter(hamburgyear, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_year.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- nrow(n)
      }
    }

    if(global$location == "Frankfurt"){
      if(global$period == "three-days"){
        count <- nrow(filter(frankfurtdays, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_days.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
        
      }
      if(global$period == "weekly"){
        count <- nrow(filter(frankfurtweek, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_week.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
        
      }
      if(global$period == "monthly"){
        count <- nrow(filter(frankfurtmonth, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_month.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "yearly"){
        count <- nrow(filter(frankfurtyear, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_year.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
    }

    if(global$location == "Cologne"){
      if(global$period == "three-days"){
        count <- nrow(filter(colognedays, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_days.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "weekly"){
        count <- nrow(filter(cologneweek, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_week.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "monthly"){
        count <- nrow(filter(colognemonth, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_month.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "yearly"){
        count <- nrow(filter(cologneyear, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_year.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
    }

    if(global$location == "Munich"){
      if(global$period == "three-days"){
        count <- nrow(filter(munichdays, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_days.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "weekly"){
        count <- nrow(filter(munichweek, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_week.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "monthly"){
        count <- nrow(filter(munichmonth, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_month.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- length(n)
      }
      if(global$period == "yearly"){
        count <- nrow(filter(munichyear, avg_p1>25))
        n <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_year.csv")
        n$sensor_id <- as.factor(n$sensor_id)
        n <- unique(n$sensor_id)
        n <- nrow(n)
      }
    }

  if(global$period == "yearly")
    {paste("<font size=+1> <B> The german government's set limit for air pollution is exceeded for ",count,"days. </B>  </font><br> <br> <center> <font size=+2> Air pollution in 25&microg/m&sup3 during the seleted timeframe </font> </center> Aggregated daily <br> Sensors used: ",n)}
  else {paste("<font size=+1> <B> The german government's set limit for air pollution is exceeded for ",count,"hours. </B> </font> <br> <br> <center> <font size=+2> Air pollution in 25&microg/m&sup3 during the selected timeframe</font> </center> Aggregated hourly <br> Sensors used: ",n)}
  
   
  })
  
  
  
  #end code here
  
}

