## Helper File for pre-calculating the result dataframes
library(dplyr)

#Read CSV Files
temp <- tempfile()
download.file("http://download-data.deutschebahn.com/static/datasets/flinkster/20170516/OPENDATA_BOOKING_CARSHARING.zip",temp)
booking <- read.csv2(unzip(temp), sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
#booking <- read.csv2("C:\\Flinkster_Open_Data\\OPENDATA_BOOKING_CARSHARING.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
unlink(c(temp,"./OPENDATA_BOOKING_CARSHARING.csv"))
rm(temp)

temp <- tempfile()
download.file("http://download-data.deutschebahn.com/static/datasets/flinkster/20170516/OPENDATA_CATEGORY_CARSHARING.zip",temp)
category <- read.csv2(unzip(temp), sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
#category <- read.csv2("C:\\Flinkster_Open_Data\\OPENDATA_CATEGORY_CARSHARING.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
unlink(c(temp,"./OPENDATA_CATEGORY_CARSHARING.csv"))
rm(temp)

temp <- tempfile()
download.file("http://download-data.deutschebahn.com/static/datasets/flinkster/20170516/OPENDATA_RENTAL_ZONE_CARSHARING.zip",temp)
rentalZone <- read.csv2(unzip(temp), sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
#rentalZone <- read.csv2("C:\\Flinkster_Open_Data\\OPENDATA_RENTAL_ZONE_CARSHARING.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
unlink(c(temp,"./OPENDATA_RENTAL_ZONE_CARSHARING.csv"))
rm(temp)

temp <- tempfile()
download.file("http://download-data.deutschebahn.com/static/datasets/flinkster/20170516/OPENDATA_VEHICLE_CARSHARING.zip",temp)
vehicle <- read.csv2(unzip(temp), sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
#vehicle <- read.csv2("C:\\Flinkster_Open_Data\\OPENDATA_VEHICLE_CARSHARING.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
unlink(c(temp,"./OPENDATA_VEHICLE_CARSHARING.csv"))
rm(temp)

#Filter vehicles with 0 kw - we assume that this is NAVI 
vehicle <- vehicle[vehicle$KW!=0,]

#Filter vehicles with fuel type "Erdgas (Nottank: Super)" -> never booked, not relevant
vehicle <- vehicle[vehicle$FUEL_TYPE_NAME!="Erdgas (Nottank: Super)",]

#Combine vehicles with fuel type "Super (Benzin)" and "Super E10" to "Benzin"
vehicle$FUEL_TYPE_NAME[vehicle$FUEL_TYPE_NAME=="Super E10"|vehicle$FUEL_TYPE_NAME=="Super (Benzin)"] <- "Gasoline"

#Rename of fuel type "Plug In (Strom, Super)" to "PlugIn Hybrid" 
vehicle$FUEL_TYPE_NAME[vehicle$FUEL_TYPE_NAME=="Plug In (Strom, Super)"] <- "PlugIn Hybrid"
vehicle$FUEL_TYPE_NAME[vehicle$FUEL_TYPE_NAME=="Strom"] <- "Electricity"

#Define vehicle groups depending on the kw column 
vehicle$VEHICLE_KW_GROUP <- vehicle$KW

#Remove all Bookings with distance == 0 or distance == NA
booking <- booking[!(is.na(booking$DISTANCE)|booking$DISTANCE==0),]
#Rename CITY_RENTAL_ZONEs with names from global dashboard
booking$CITY_RENTAL_ZONE[booking$CITY_RENTAL_ZONE=="Flugh. Berlin"] <- "Berlin"
booking$CITY_RENTAL_ZONE[booking$CITY_RENTAL_ZONE=="München"] <- "Munich"
booking$CITY_RENTAL_ZONE[booking$CITY_RENTAL_ZONE=="Köln"] <- "Cologne"
booking$CITY_RENTAL_ZONE[booking$CITY_RENTAL_ZONE=="Frankfurt am Main"] <- "Frankfurt"

#extract rental zones for markers in global map
flinksterMarkerDF <- rentalZone
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="Flugh. Berlin"]     <- "Berlin"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="München"]           <- "Munich"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="München (DB-intern)"] <- "Munich"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="Flugh. München"]    <- "Munich"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="Köln"]              <- "Cologne"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="Frankfurt am Main"] <- "Frankfurt"
flinksterMarkerDF$CITY[flinksterMarkerDF$CITY=="Flugh. Frankfurt"] <- "Frankfurt"

#cleaning of wrong lng/lat values   
#     "loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
#     "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
#     "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))

citys <- data.frame("loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
                    "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
                    "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))

#iterative logic to identify stations with a high deviation
delete <- NULL
for(city in 1:nrow(citys)) {
  tmp <- flinksterMarkerDF[flinksterMarkerDF$CITY==citys$loc[city],]
  tmp[is.na(tmp)] <- 0
  defValue <- c(citys$lng[city],citys$lat[city])
  for (row in 1:nrow(tmp)) {
    if (abs(tmp$LONGITUDE[row]-defValue[1])>2 | abs(tmp$LATITUDE[row]-defValue[2])>2) {
      delete <- c(delete,tmp$RENTAL_ZONE_HAL_ID[row])
    }
  }
}

flinksterMarkerDF <- flinksterMarkerDF[-(which(flinksterMarkerDF$RENTAL_ZONE_HAL_ID %in% delete)),]
rm(defValue, delete, row)

flinksterMarkerDF <- flinksterMarkerDF[flinksterMarkerDF$CITY=='Berlin'|flinksterMarkerDF$CITY=='Munich'|flinksterMarkerDF$CITY=='Cologne'|flinksterMarkerDF$CITY=='Frankfurt'|flinksterMarkerDF$CITY=='Hamburg',]%>%
  select(CITY,RENTAL_ZONE_HAL_ID,NAME,TYPE,LATITUDE,LONGITUDE)
colnames(flinksterMarkerDF) <- c("loc","RENTAL_ZONE_HAL_ID","NAME","TYPE","lat","lng")

#Building groups for distances
#Summarize all bookings with distance groups > 200
#Median only needs ordinal scale measurements so we summarize all distances larger than 200 kilometers to one group
booking$DISTANCE[booking$DISTANCE > 200] <- 200

#Join vehicle information
globalDF <- inner_join(booking,vehicle,by ="VEHICLE_HAL_ID")

#Join category information
globalDF <- left_join(globalDF,category,by = c("CATEGORY_HAL_ID"= "HAL_ID"))

#Join rental zone information
#start rental zone
tempRentalZone <- rentalZone
colnames(tempRentalZone) <- paste("START", colnames(rentalZone), sep="_")
#Left Join to get further information about known rentalzones
#There are rentalzones in bookings which are not available in the rentalzones table
globalDF <- left_join(globalDF,tempRentalZone,by = "START_RENTAL_ZONE_HAL_ID")

#end rental zone
tempRentalZone <- rentalZone
colnames(tempRentalZone) <- paste("END", colnames(rentalZone), sep="_")
#Left Join to get further information about known rentalzones
#There are rentalzones in bookings which are not available in the rentalzones table
globalDF <- left_join(globalDF,tempRentalZone,by = "END_RENTAL_ZONE_HAL_ID")
rm(tempRentalZone)

#Change type from column in globalDF
globalDF$DATE_FROM <- as.character.Date(globalDF$DATE_FROM)

#Delete unnecessary tables
rm(booking,category,rentalZone,vehicle)

# Save globalDF Table object
#save(globalDF, file = "C:\\Flinkster_Open_Data\\globalDF.RData")
# Load globalDF Table in Workspace
#load("C:\\Flinkster_Open_Data\\globalDF.RData")

citys <- c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich")
threeDays <- c("2016-08-29 00:00", "2016-08-31 23:59")
weekly <- c("2016-08-25 00:00", "2016-08-31 23:59")
monthly <- c("2016-08-01 00:00", "2016-08-31 23:59")
yearly <- c("2016-01-01 00:00", "2016-12-31 23:59")

timespans <- c("threeDays", "weekly", "monthly", "yearly")

for(city in citys){
  globalDF_city_filtered <- globalDF[globalDF$CITY_RENTAL_ZONE==city,]
  bigresultDF <- NULL
  for(time in timespans){
    globalDF_filtered <- globalDF_city_filtered[globalDF_city_filtered$DATE_FROM>=eval(parse(text=paste0(time,"[1]")))
                                                &globalDF_city_filtered$DATE_FROM<=eval(parse(text=paste0(time,"[2]"))),]
    
    #Grouping on columns FUEL_TYPE_NAME and VEHICLE_KW_GROUP to calculate 
    resultDF <- globalDF_filtered %>% group_by(globalDF_filtered$FUEL_TYPE_NAME, globalDF_filtered$VEHICLE_KW_GROUP) %>% summarize(NBR_OF_BOOKINGS=n(),MEDIAN_KM_VEHICLE_GROUP=median(DISTANCE))
    
    #Adding Timemarker to each row of the dataframe
    resultDF <- cbind(as.data.frame(rep(time,nrow(resultDF))),as.data.frame(resultDF))
    colnames(resultDF) <- c("TIMESPAN","FUEL_TYPE_NAME", "VEHICLE_KW_GROUP", "NBR_OF_BOOKINGS", "MEDIAN_KM_VEHICLE_GROUP")
    
    #saving temorary results
    bigresultDF <- rbind(bigresultDF, resultDF)
    
    rm(resultDF)
  }
  assign(paste("resultDF",city, sep = '_'), bigresultDF)
  
  #Grouping on columns FUEL_TYPE_NAME on the globalDF_city_filtered to calculate median and NBR of Bookings
  resultDF_bar_pie <- globalDF_city_filtered %>% group_by(globalDF_city_filtered$FUEL_TYPE_NAME) %>% 
    summarize(MEDIAN_KM_VEHICLE_GROUP=median(DISTANCE),NBR_OF_BOOKINGS=n())
  #Calculating Median over all datasets and adding it to the resultDF_bar_pie
  resultDF_bar_pie$OVERALL_MEDIAN <- median(globalDF_city_filtered$DISTANCE)
  colnames(resultDF_bar_pie) <- c("FUEL_TYPE_NAME", "MEDIAN_KM_VEHICLE_GROUP", "NUMBER_OF_BOOKINGS","OVERALL_MEDIAN")
  #Extend the dataframe with not available fueltypes
  if(city == "Cologne"|city == "Munich"){
    tmp <- as.data.frame(c("Electricity","PlugIn Hybrid"),stringsAsFactors = FALSE)
    colnames(tmp) <- "FUEL_TYPE_NAME"
    resultDF_bar_pie <- bind_rows(resultDF_bar_pie,tmp)
    resultDF_bar_pie[is.na(resultDF_bar_pie)] <- 0
  }
  # Create result data frame for bar and pie charts
  assign(paste("resultDF_bar_pie",city, sep = '_'), resultDF_bar_pie)
  
  rm(globalDF_city_filtered,bigresultDF,globalDF_filtered,resultDF_bar_pie)
}

#Saving pre-calculated results
save(resultDF_Berlin,resultDF_Frankfurt,resultDF_Hamburg,resultDF_Cologne,resultDF_Munich,resultDF_bar_pie_Berlin,resultDF_bar_pie_Cologne,resultDF_bar_pie_Frankfurt,resultDF_bar_pie_Hamburg,resultDF_bar_pie_Munich,flinksterMarkerDF,threeDays,weekly,monthly,yearly, file = "datafiles\\results_Flinkster.RData")

rm(city,citys,time,timespans,threeDays,monthly,weekly,yearly,globalDF, tmp)