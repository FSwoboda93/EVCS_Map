#file from group5: Luftdaten
#read in data with sensor positions depending on selection

sensormarkers <- function (global){
  
  if(global$location == "Berlin"){
    if(global$period == "three-days"){
      sensors <-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_days.csv")
    }
    if(global$period == "weekly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_week.csv")
    }
    if(global$period == "monthly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_month.csv")
    }
    if(global$period == "yearly"){
       sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/berlin_sensors_year.csv")
    }
  }
  
  if(global$location == "Cologne"){
    if(global$period == "three-days"){
      sensors <-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_days.csv")
    }
    if(global$period == "weekly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_week.csv")
    }
    if(global$period == "monthly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_month.csv")
    }
    if(global$period == "yearly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/cologne_sensors_year.csv")
    }
  }
  
  if(global$location == "Frankfurt"){
    if(global$period == "three-days"){
      sensors <-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_days.csv")
    }
    if(global$period == "weekly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_week.csv")
    }
    if(global$period == "monthly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_month.csv")
    }
    if(global$period == "yearly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/frankfurt_sensors_year.csv")
    }
  }
  
  if(global$location == "Hamburg"){
    if(global$period == "three-days"){
      sensors <-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_days.csv")
    }
    if(global$period == "weekly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_week.csv")
    }
    if(global$period == "monthly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_month.csv")
    }
    if(global$period == "yearly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/hamburg_sensors_year.csv")
    }
  }
  
  if(global$location == "Munich"){
    if(global$period == "three-days"){
      sensors <-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_days.csv")
    }
    if(global$period == "weekly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_week.csv")
    }
    if(global$period == "monthly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_month.csv")
    }
    if(global$period == "yearly"){
      sensors<-read.csv2("../2018-ws-mobility-dashboard/datafiles/Luftdaten/sensors/munich_sensors_year.csv")
    }
  }
  
  return(sensors)
}