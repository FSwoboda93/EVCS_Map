
osm <- function (global){
  
  
     # osmdata <-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM_data.csv")
      if(global$location == "Berlin"){
          osmdata <-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM/OSM_data_B.csv")
        
      }
      
      if(global$location == "Cologne"){
         osmdata <-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM/OSM_data_C.csv")
        
      }
      
      if(global$location == "Frankfurt"){
        osmdata<-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM/OSM_data_F.csv")

      }
      
      if(global$location == "Hamburg"){
          osmdata <-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM/OSM_data_H.csv")
      }
      
      if(global$location == "Munich"){
          osmdata <-read.csv2("../2018-ws-mobility-dashboard/datafiles/OSM/OSM_data_M.csv")
      }
  
  return(osmdata)
}