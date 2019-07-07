# file from group5: Luftdaten
#data preparation for year data. Depending on selected data, a data file for a month is read in. 
#Start at reading in relevant data from source
#Next: prepare data and split into raw data for specific city.
#then: calculate mean out of raw data to get data with average pollution, rounded by day, form each city from 1 month
#then2: get unique sensor ids from each city for the read in month.
#output: berlin3, frankfurt3, cities3,... where chars describe city, and the number describes row [change numbers before running a new month].
#cities3 is the average(mean) of the cities from month march in this case.
#output2: unique sensors with lat, lon from the 5 cities for specific month

library(dplyr)
library(lubridate)
library(data.table)
library(geosphere)
library(readr)

coords <- data.frame("loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
                     "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
                     "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))

cityrange <- 30000

#read in datafile with relevant data
#look closely how data is interpreted!
#depending on interpretation of readr, adjust lat/lon for precalculation. col() doesnt interpret datatypes for the subset.
#col_number throws out the comma, adjust with /1000. col_chararacter() -> as numeric() keeps the decimals
f <- function(x, pos) subset(x, subset = 
                               between(lat/1000, coords$lat[1]-0.5, coords$lat[1]+0.5) & between(lon/1000,coords$lng[1]-0.5, coords$lng[1]+0.5)
                              |between(lat/1000, coords$lat[2]-0.5, coords$lat[2]+0.5) & between(lon/1000,coords$lng[2]-0.5, coords$lng[2]+0.5)
                              |between(lat/1000, coords$lat[3]-0.5, coords$lat[3]+0.5) & between(lon/1000,coords$lng[3]-0.5, coords$lng[3]+0.5)
                              |between(lat/1000, coords$lat[4]-0.5, coords$lat[4]+0.5) & between(lon/1000,coords$lng[4]-0.5, coords$lng[4]+0.5)
                              |between(lat/1000, coords$lat[5]-0.5, coords$lat[5]+0.5) & between(lon/1000,coords$lng[5]-0.5, coords$lng[5]+0.5)
                             
                             ,  select= c(sensor_id, lat, lon, timestamp, P2 ))

#luftdaten <- readr::read_csv2_chunked("../../Desktop/data_year/2018-02_sds011.csv", DataFrameCallback$new(f), chunk_size = 10000000, col_names = TRUE)
#subset = distHaversine(cbind(coords$lat[1], coords$lng[1]),cbind(as.numeric(lat), as.numeric(lon))) < 30000
#subset = between(lat/1000, coords$lat[2]-0.5, coords$lat[2]+0.5) & between(as.numeric(lon),coords$lng[2]-0.5, coords$lng[2]+0.5)
#f <- function(x, pos) subset(x,  subset = distHaversine(cbind(coords$lat[1], coords$lng[1]),cbind(as.numeric(as.character(lat)), as.numeric(lon))) < 30000, select= c(sensor_id, lat, lon, timestamp, P2 ))

luftdaten <- readr::read_csv2_chunked("../Desktop/data/2018-03_sds011.csv", DataFrameCallback$new(f), chunk_size = 10000000,
                                   col_names = TRUE)

#fix values to calculate distance later
luftdaten
luftdaten$sensor_id <- as.factor(luftdaten$sensor_id)
luftdaten$lat <- round(luftdaten$lat/1000, digits =3)
luftdaten$lon <- round(luftdaten$lon/1000, digits =3)
#luftdaten$lon <- as.numeric(as.character(luftdaten$lon))
luftdaten$timestamp<- ymd_hms(luftdaten$timestamp, tz = "UTC")
luftdaten$timestamp <- round_date(luftdaten$timestamp, unit ="day")
luftdaten$P2 <- as.numeric(as.character(luftdaten$P2))
luftdaten<- na.omit(luftdaten)

#split data according to cities
raw_berlin_month <- filter(luftdaten, distHaversine(cbind(coords$lat[1], coords$lng[1]),cbind(lat, lon)) < cityrange)
raw_hamburg_month <- filter(luftdaten, distHaversine(cbind(coords$lat[2], coords$lng[2]),cbind(lat, lon)) < cityrange)
raw_frankfurt_month <- filter(luftdaten, distHaversine(cbind(coords$lat[3], coords$lng[3]),cbind(lat, lon)) < cityrange)
raw_cologne_month <- filter(luftdaten, distHaversine(cbind(coords$lat[4], coords$lng[4]),cbind(lat, lon)) < cityrange)
raw_munich_month <- filter(luftdaten, distHaversine(cbind(coords$lat[5], coords$lng[5]),cbind(lat, lon)) < cityrange)

#create and write mean(avg) for month that is read in
avg_berlin_month <- raw_berlin_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_berlin_month
write.csv2(avg_berlin_month,file ="../Desktop/output/berlin3.csv")

avg_hamburg_month <- raw_hamburg_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_hamburg_month
write.csv2(avg_hamburg_month,file ="../Desktop/output/hamburg3.csv")

avg_frankfurt_month <- raw_frankfurt_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_frankfurt_month
write.csv2(avg_frankfurt_month,file ="../Desktop/output/frankfurt3.csv")

avg_cologne_month <- raw_cologne_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_cologne_month
write.csv2(avg_cologne_month,file ="../Desktop/output/cologne3.csv")

avg_munich_month <- raw_munich_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_munich_month
write.csv2(avg_munich_month,file ="../Desktop/output/munich3.csv")


#create and write city average for this month
avg_cities_month <- bind_rows(avg_berlin_month,avg_cologne_month,avg_frankfurt_month,avg_hamburg_month,avg_munich_month)
avg_cities_month
avg_cities_month <- avg_cities_month %>% arrange(avg_cities_month$timestamp) %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
tail(avg_cities_month)
write.csv2(avg_cities_month,file ="../Desktop/output/cities3.csv")

#create and write dataframe for sensors
raw_berlin_month
berlin_sensors_month <- raw_berlin_month %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(raw_berlin_month$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
berlin_sensors_month <- unique(berlin_sensors_month, incomparables = FALSE)
berlin_sensors_month
write.csv2(berlin_sensors_month,file ="../Desktop/output/sensors_berlin3.csv")

hamburg_sensors_month <- raw_hamburg_month %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(raw_hamburg_month$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
hamburg_sensors_month <- unique(hamburg_sensors_month, incomparables = FALSE)
hamburg_sensors_month
write.csv2(hamburg_sensors_month,file ="../Desktop/output/sensors_hamburg3.csv")

frankfurt_sensors_month <- raw_frankfurt_month %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(raw_frankfurt_month$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
frankfurt_sensors_month <- unique(frankfurt_sensors_month, incomparables = FALSE)
frankfurt_sensors_month
write.csv2(frankfurt_sensors_month,file ="../Desktop/output/sensors_frankfurt3.csv")

cologne_sensors_month <- raw_cologne_month %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(raw_cologne_month$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
cologne_sensors_month <- unique(cologne_sensors_month, incomparables = FALSE)
cologne_sensors_month
write.csv2(cologne_sensors_month,file ="../Desktop/output/sensors_cologne3.csv")

munich_sensors_month <- raw_munich_month %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(raw_munich_month$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
munich_sensors_month <- unique(munich_sensors_month, incomparables = FALSE)
munich_sensors_month
write.csv2(munich_sensors_month,file ="../Desktop/output/sensors_munich3.csv")


