#file from group5: Luftdaten
# calculate a mean of focused city for specific timeframe in order to colorise circle
circlecolor <- function (global){
  
  if(global$location == "Berlin"){
    if(global$period == "three-days"){
      berlindays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_days.csv")
      berlindays$P2 <- as.numeric(as.character(berlindays$P2))
      meanpollution <- mean(berlindays$P2, na.rm = TRUE)
    }
    if(global$period == "weekly"){
      berlinweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_week.csv")
      berlinweek$P2 <- as.numeric(as.character(berlinweek$P2))
      meanpollution <- mean(berlinweek$P2, na.rm = TRUE)
    }
    if(global$period == "monthly"){
      berlinmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_month.csv")
      berlinmonth$P2 <- as.numeric(as.character(berlinmonth$P2))
      meanpollution <- mean(berlinmonth$P2, na.rm = TRUE)
    }
    if(global$period == "yearly"){
      berlinyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_berlin_year.csv")
      berlinyear$P2 <- as.numeric(as.character(berlinyear$P2))
      meanpollution <- mean(berlinyear$P2, na.rm = TRUE)
    }
  }
  if(global$location == "Hamburg"){
    if(global$period == "three-days"){
      hamburgdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_days.csv")
      hamburgdays$P2 <- as.numeric(as.character(hamburgdays$P2))
      meanpollution <- mean(hamburgdays$P2, na.rm = TRUE)
    }
    if(global$period == "weekly"){
      hamburgweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_week.csv")
      hamburgweek$P2 <- as.numeric(as.character(hamburgweek$P2))
      meanpollution <- mean(hamburgweek$P2, na.rm = TRUE)
    }
    if(global$period == "monthly"){
      hamburgmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_month.csv")
      hamburgmonth$P2 <- as.numeric(as.character(hamburgmonth$P2))
      meanpollution <- mean(hamburgmonth$P2, na.rm = TRUE)
    }
    if(global$period == "yearly"){
      hamburgyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_hamburg_year.csv")
      hamburgyear$P2 <- as.numeric(as.character(hamburgyear$P2))
      meanpollution <- mean(hamburgyear$P2, na.rm = TRUE)
    }
  }
  
  if(global$location == "Frankfurt"){
    if(global$period == "three-days"){
      frankfurtdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_days.csv")
      frankfurtdays$P2 <- as.numeric(as.character(frankfurtdays$P2))
      meanpollution <- mean(frankfurtdays$P2, na.rm = TRUE)
    }
    if(global$period == "weekly"){
      frankfurtweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_week.csv")
      frankfurtweek$P2 <- as.numeric(as.character(frankfurtweek$P2))
      meanpollution <- mean(frankfurtweek$P2, na.rm = TRUE)
    }
    if(global$period == "monthly"){
      frankfurtmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_month.csv")
      frankfurtmonth$P2 <- as.numeric(as.character(frankfurtmonth$P2))
      meanpollution <- mean(frankfurtmonth$P2, na.rm = TRUE)
    }
    if(global$period == "yearly"){
      frankfurtyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_frankfurt_year.csv")
      frankfurtyear$P2 <- as.numeric(as.character(frankfurtyear$P2))
      meanpollution <- mean(frankfurtyear$P2, na.rm = TRUE)
    }
  }
  
  if(global$location == "Cologne"){
    if(global$period == "three-days"){
      colognedays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_days.csv")
      colognedays$P2 <- as.numeric(as.character(colognedays$P2))
      meanpollution <- mean(colognedays$P2, na.rm = TRUE)
    }
    if(global$period == "weekly"){
      cologneweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_week.csv")
      cologneweek$P2 <- as.numeric(as.character(cologneweek$P2))
      meanpollution <- mean(cologneweek$P2, na.rm = TRUE)
    }
    if(global$period == "monthly"){
      colognemonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_month.csv")
      colognemonth$P2 <- as.numeric(as.character(colognemonth$P2))
      meanpollution <- mean(colognemonth$P2, na.rm = TRUE)
    }
    if(global$period == "yearly"){
      cologneyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_cologne_year.csv")
      cologneyear$P2 <- as.numeric(as.character(cologneyear$P2))
      meanpollution <- mean(cologneyear$P2, na.rm = TRUE)
    }
  }
  
  if(global$location == "Munich"){
    if(global$period == "three-days"){
      munichdays <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_days.csv")
      munichdays$P2 <- as.numeric(as.character(munichdays$P2))
      meanpollution <- mean(munichdays$P2, na.rm = TRUE)
    }
    if(global$period == "weekly"){
      munichweek <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_week.csv")
      munichweek$P2 <- as.numeric(as.character(munichweek$P2))
      meanpollution <- mean(munichweek$P2, na.rm = TRUE)
    }
    if(global$period == "monthly"){
      munichmonth <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_month.csv")
      munichmonth$P2 <- as.numeric(as.character(munichmonth$P2))
      meanpollution <- mean(munichmonth$P2, na.rm = TRUE)
    }
    if(global$period == "yearly"){
      munichyear <- read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/avg/avg_munich_year.csv")
      munichyear$P2 <- as.numeric(as.character(munichyear$P2))
      meanpollution <- mean(munichyear$P2, na.rm = TRUE)
    }
  }
  
  colours <- c("#A9DFFF","#78CDFF","#0EA5FF","#00578A","#00436A")
  anticolours <- c("#FFd6A4","#FFBf70","#FF8C00","#D97700","#A75C00")
  
  if(meanpollution<5){return(colours[1])}
  if(meanpollution<10&&meanpollution>=5){return(colours[2])}
  if(meanpollution<15&&meanpollution>=10){return(colours[3])}
  if(meanpollution<20&&meanpollution>=15){return(colours[4])}
  if(meanpollution<25&&meanpollution>=20){return(colours[5])}
  if(meanpollution<30&&meanpollution>=25){return(anticolours[1])}
  if(meanpollution<35&&meanpollution>=30){return(anticolours[2])}
  if(meanpollution<40&&meanpollution>=35){return(anticolours[3])}
  if(meanpollution<45&&meanpollution>=40){return(anticolours[4])}
  if(meanpollution<45){return(anticolours[5])}
  
}