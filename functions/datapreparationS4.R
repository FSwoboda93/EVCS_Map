## data preparation for BI project: group5 Luftdaten
## this file is filtering,selecting and preparing data
## to generate small data samples that can be used
## for plotting graphs in the dashboard
##
## For questions or required data, email to Clemens Eyring. 



library(dplyr)
library(lubridate)
library(data.table)
library(geosphere)
library(readr)

##########################
### november/december ####
##########################

## read files, add row descriptions optionally if required.
## the input data is all data from 1 sensor type of 1 month(here oct 2018)
## workaorund required for later work- maximum file size 5MB in RStudio -> readr

# data_pms7 <- read.csv2("../dataprep/2018-10_pms7003.csv")
#data_pms3 <- read.csv2("../dataprep/2018-10_pms5003.csv")
#data_ppd <- read.csv2("../dataprep/2018-10_ppd42ns.csv")

# head(data_pms7)
# summary(data_pms7)
# tail(data_pms7)
#str(data_pms7)

##we decided to focus on pms7003 sensor data at first: decent number of fitting sensors + data


##prepare data(no mising values/outliers)

# data_pms7$sensor_id <- as.factor(data_pms7$sensor_id)
# data_pms7$timestamp<- ymd_hms(data_pms7$timestamp)
# data_pms7$P1 <- as.numeric(as.character(data_pms7$P1))
# data_pms7$P2 <- as.numeric(as.character(data_pms7$P2))
# data_pms7$P0 <- as.numeric(as.character(data_pms7$P0))
# head(data_pms7)
# data_pms7$timestamp <- round_date(data_pms7$timestamp, unit ="hour")
# data_pms7$timestamp
# pms <- data_pms7 %>% group_by(timestamp) %>% summarise(P1 = mean(P1))
# pms
# 
# summary(data_pms7$sensor_id, maxsum = 100)

#Ids are 8761 11813 12246 13030 14880 16397 16632 -> 5 required(1 for each city)
# sensors <- c("8761","13030","14880","16632","16397" )
# soon: selection of sensors by lat/lon
#further note: time doesnt matter so far(Andreas), so we will choose the timeframe from 1.10.18 to 8.10.18(mday)

#filter(data_pms7, sensor_id ==14880)
##df[(df['dt'] > '2014-07-23 07:30:00') & (df['dt'] < '2014-07-23 09:00:00')]

## generate dataframe for a day/week for 1 sensor
# day <-data_pms7[(data_pms7['timestamp'] >= '2018-10-01 00:00:00') &(data_pms7['timestamp'] <= '2018-10-04 02:00:00'),]
# day
# tail(day)
# daydata <- filter(data_pms7, mday(data_pms7$timestamp) ==1 & sensor_id ==14880)
#daydata
# weekdata <- filter(data_pms7, mday(data_pms7$timestamp) %in%1:8 & sensor_id ==14880)
#weekdata

##write dataframe of a specific day/week
# write.csv2(daydata,file ="../dataprep/sample_day.csv")
# write.csv2(weekdata,file ="../dataprep/sample_week.csv")

##generate whole relevant dataframe -> select only relevant rows

# relevant_data <- data_pms7 %>% select( sensor_id, timestamp, P1, P2) %>% filter(sensor_id %in% sensors &mday(timestamp)%in% 1:8)
# summary(relevant_data)
# 
# day8761 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="8761")
# day8761
# week8761 <- filter(relevant_data, sensor_id =="8761")
# 
# day13030 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="13030")
# day13030
# week13030 <- filter(relevant_data, sensor_id =="13030")
# 
# day16632 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="16632")
# week16632 <- filter(relevant_data, sensor_id =="16632")
# 
# day16397 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="16397")
# week16397 <- filter(relevant_data, sensor_id =="16397")
# 
# day14880 <-filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="14880")
# week14880 <- filter(relevant_data, sensor_id =="14880")
#   
# day14880 <- create_avg_data(day14880,4)
# week14880 <- create_avg_data(week14880,9)
# 
# write.csv2(day14880,file ="../dataprep/day14880.csv")
# write.csv2(week14880,file ="../dataprep/week14880.csv")
# 
# 
# day13030 <- create_avg_data(day13030,5)
# week13030 <- create_avg_data(week13030,12)
# 
# write.csv2(day8761,file ="../dataprep/day8761.csv")
# write.csv2(week8761,file ="../dataprep/week8761.csv")
# write.csv2(day13030,file ="../dataprep/day13030.csv")
# write.csv2(week13030,file ="../dataprep/week13030.csv")
# write.csv2(day16632,file ="../dataprep/day16632.csv")
# write.csv2(week16632,file ="../dataprep/week16632.csv")
# write.csv2(day16397,file ="../dataprep/day16397.csv")
# write.csv2(week16397,file ="../dataprep/week16397.csv")

# write.csv2(relevant_data,file ="../dataprep/wholedata.csv")
#calculate average for every 10min/15 min/30min/hour
#also use this function for total average(relevant_data)

#relevant_data[,list(avg=mean(relevant_data$P2), mday(data_pms7$timestamp))]
# summary(relevant_data)

# or just trim data by mean of every n rows

# small_data <- head(daydata)
# small_data
# month(small_data$timestamp)
# minute(small_data$timestamp)
# second(small_data$timestamp)

#n()
#x <- sample(1:10, 1e5, rep = TRUE)
#length(unique(x))
#n_distinct(small_data)

# mean(small_data$P1)
# summarise(small_data, sensor_id, meanP1= mean(P1), mean_timestamp= mean(timestamp))
# 
# summarise(small_data, avg= mean(timestamp))

#calc_avg <- function(data_frame, n){
#  
#  counter <- n()
#  
#  while(<condition>){<instructions>}
#}
# small_data
#small_data %>% 
#  group_by(grp=cumsum(c(TRUE, diff(start)>5))) %>%
 # summarise(start=start[1], end=end[n()], P1=mean(P1))%>%
#  select(-grp)

#setDT(small_data)[,.(P1= mean(P1)),timestamp-0:1]

#n=2
#small_data$group <- rep(1:5, each = n)
#small_data
#setDT(small_data)
#res <- small_data[, .timestamp =min(timestamp), mean_P1= mean(P1), by = group]
# trim=2
# small_data[which(1:nrow(small_data) %% trim == 1) , ]
# n=2
# small_data[1:(nrow(small_data)/n),]
# mean(small_data$P1, by=group )
#summarise(small_data, avg=mean(P1), by=group)










#num_first <-1
#step <- 2
#numrow <- 3
#avg_date <- ymd_hms(c())
#avg_p1 <- c()
#nrow(small_data)
#for(i in 1:nrow(small_data))

# create_avg_data <- function(db, step=2){
#   
#   num_first <-1
#   numrow <- 1+ step
#   avg_date <- ymd_hms(c(), tz = "CET")
#   avg_p1 <- c()
#   new_data <-data.frame("avg_timestamp" =avg_date, "avg_p1" =avg_p1)
#   
  #nrow(data)
  
  # while(numrow <=nrow(db)){
    
    #print(mean(data[num_first:numrow,]$P1))
    #print(mean(data[num_first:numrow,]$timestamp))
    #db <- day8761
    # avg_p1 <-  mean(db[num_first:numrow,]$P1)
    # avg_date <- mean(db[num_first:numrow,]$timestamp)
    # nrow <- data.frame(avg_date, avg_p1)
    # new_data <- rbind(new_data, nrow)
    # 
    # 
    # num_first <- num_first + step+1
    # numrow <- numrow + step+1
    
  # }
  # print(new_data)
  #avg_p1
  # return (new_data)
  #+ create dataset 
  #new_data <-data.frame("avg_timestamp" =avg_date, "avg_p1" =avg_p1)
  
  
  
#   
# }
# 
# avgday8761 <-create_avg_data(day8761)
# avgday8761
# 
# avg_week <- create_avg_data(relevant_data,33)
# avg_week
# write.csv2(avg_week, file ="../dataprep/avg_week.csv")

# relevant_day <- relevant_data%>% filter(mday(relevant_data$timestamp)==1)
# relevant_day
# 
# 
# relevant_day <- create_avg_data(relevant_day, 11)
# write.csv2(relevant_day, file ="../dataprep/avg_day.csv" )

#while(numrow <=nrow(small_data)){
#  
#  print(mean(small_data[num_first:numrow,]$P1))
#  print(mean(small_data[num_first:numrow,]$timestamp))
#
#  avg_p1 <- c(avg_p1, mean(small_data[num_first:numrow,]$P1))
#  avg_date <- c(avg_date, mean(small_data[num_first:numrow,]$timestamp))
#  num_first <- num_first + step
#  numrow <- numrow + step
#  
#}

#avg_date
#avg_p1

#num_first <-1
#step <- 2
#numrow <- 3
#mean(small_data[num_first:numrow,]$P1)
#mean(small_data[num_first:numrow,]$timestamp)


######################################################################
######################################################################
######################################################################


##############################
### january (Sprint 4) #######
##############################


coords <- data.frame("loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
                     "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
                     "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))
  

distHaversine(c(8.682127, 50.110922),c(11.5819806, 48.1351253))  
distm(c(8.682127, 50.110922), c(11.5819806, 48.1351253), fun = distHaversine)


# #does not work
# library(data.table)
# yyy = fread("../../Desktop/data/2018-10_sds011/2018-10_sds011.csv", header = TRUE)
# 
# 
# #weird matrice
# library(bigmemory)
# y <- read.big.matrix("../../Desktop/data/2018-10_sds011/2018-10_sds011.csv", type = "integer", header=TRUE)
# dim(y)
# 
# nrow(y)
# ncol(y)
# #coerce a big.matrix to a matrix
# yy= as.matrix(y)



library(geosphere)   
library(tidyverse)

# Fake data
#set.seed(2)
#dat = data.frame(lon=runif(5,-180,180), lat = runif(5,-90,90))
#dat
#dat[,1]
#dat <- data.frame(t(combn(1:nrow(dat), 2)))
#dat[.x,]
#
#dat2 <- data.frame(dat[.x, ] %>% set_names(c("lon1","lat1")), 
#                    dat[.y, ] %>% set_names(c("lon2", "lat2")))
#dat2
#dist = pmap_df(data.frame(t(combn(1:nrow(dat), 2))), 
#               ~data.frame(dat[.x, ] , 
#                            
#                           dist=distHaversine(c(8.682127, 50.110922), dat[.x, ])))
#
#dat[.x,]
#
#dist
#
#
#apply(dat[-2, ], 1, function(ll) distHaversine(dat[2,], ll))
#apply(dat[,], 1, function(xx) distHaversine(c(52.520007, 13.404954 ), (xx)/1000))




library(readr)
#"../dataprep/2018-10_pms7003.csv"
#distHaversine(c(8.682127, 50.110922),c(lat, lon)) 
#distHaversine(c(8.682127, 50.110922),c(lat[pos]/1000, lon[pos]/1000))<6000000
#distHaversine(c(8.682127, 50.110922),c(rowwise(lat)/1000, rowwise(lon)/1000))
#subset(x, apply(x, 1, function(xx) distHaversine(c(52.520007, 13.404954 ), (as.numeric(xx))/1000))<300000 
#rowwise()
#apply(dat[,], 1, function(xx) distHaversine(c(52.520007, 13.404954 ), (xx)/1000))
#(apply(dat[,], 1, function(xx) distHaversine(c(52.520007, 13.404954 ), [x[2],]/1000)))<300000
#between(abs(lat)/1000, 50, 55) & between(abs(lon)/1000, 4.6, 6.1)
#filter
#filter(x, distHaversine(c(52.520007, 13.404954), c(lat, lon))<30000

# ff <- function(x, pos)  subset(x, between(lat/1000, coords$lat[1]-5, coords$lat[1]+5)  ,select= c(lat, lon, timestamp, P1))
# ycsv <- readr::read_csv_chunked("../dataprep/2018-10_pms7003.csv", DataFrameCallback$new(ff), chunk_size = 100000, col_names = TRUE)
# ydelim <- 
# 
# yyyreadr <- readr::read_csv2_chunked("../dataprep/2018-10_pms7003.csv", DataFrameCallback$new(ff), chunk_size = 100000, col_names = TRUE)
# yyyreadr
# yyyreadr$lat <- yyyreadr$lat/1000
# 
# dput(yyyreadr)
# 
# dput(head(yyyreadr, 10))
# 
# yyyreadr
# yyyreadr$lat
# tail(yreadr)

#write.csv2(yyyreadr,file ="../dataprep/raw_berlin_month.csv")

#distHaversine(c(8.682127, 50.110922),c(abs(lat), abs(as.numeric(lon))))
##distHaversine(c(8.682127, 50.110922),c(lat[pos]/1000, lon[pos]/1000))<6000000



###### read in, filter and write data from the large csv file ########
## note: latitude read in as double, but decimals are lost(coordinates are the same) at some time, control values

#coords$lng[1]
#coords$lat[1]-0.5
#
#here: lat= k, lon = dot. Repeat for any city by selecting []
###----
f <- function(x, pos) subset(x, subset = between(lat/1000, coords$lat[2]-0.5, coords$lat[2]+0.5) & between(as.numeric(lon),coords$lng[2]-0.5, coords$lng[2]+0.5) ,  select= c(sensor_id, lat, lon, timestamp, P2 ))

yreadr <- readr::read_csv2_chunked("../../Desktop/data/2018-10_sds011/2018-10_sds011.csv", DataFrameCallback$new(f), chunk_size = 10000000, col_names = TRUE)

yreadr
yreadr$sensor_id <- as.factor(yreadr$sensor_id)
yreadr$lat <- round(yreadr$lat/1000, digits =3)
yreadr$lon <- as.numeric(as.character(yreadr$lon))
yreadr$timestamp<- ymd_hms(yreadr$timestamp, tz = "UTC")
yreadr$P2 <- as.numeric(as.character(yreadr$P2))

dput(head(yreadr, 10))
head(yreadr)
tail(yreadr)

write.csv2(yreadr,file ="../dataprep/raw_city_month.csv")


##create week and 3 day data

#day <-data_pms7[(data_pms7['timestamp'] >= '2018-10-01 00:00:00') &(data_pms7['timestamp'] <= '2018-10-04 02:00:00'),]
days <- yreadr[(yreadr['timestamp'] >= '2018-10-02 01:30:00') &(yreadr['timestamp'] <= '2018-10-05 02:29:59'),]
head(days)
tail(days)

write.csv2(days,file ="../dataprep/raw_hamburg_days.csv")

weeks <- yreadr[(yreadr['timestamp'] >= '2018-10-02 01:30:00') &(yreadr['timestamp'] <= '2018-10-09 02:29:59'),]
head(weeks)
tail(weeks)

write.csv2(weeks,file ="../dataprep/raw_berlin_week.csv")



##create trimmed dataframes

#berlin

f <- function(x, pos) subset(x ,  select= c(sensor_id, lat, lon, timestamp, P2 ))
berlin_month <- readr::read_csv2_chunked("../dataprep/raw_berlin_month.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
head(berlin_month)
berlin_week <- readr::read_csv2_chunked("../dataprep/raw_berlin_week.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
berlin_days <- readr::read_csv2_chunked("../dataprep/raw_berlin_days.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)

berlin_month$timestamp <- round_date(berlin_month$timestamp, unit ="hour")
berlin_month$timestamp
berlin_month$P2 <- as.numeric(as.character(berlin_month$P2))
tail(berlin_month)
avg_berlin_month <- berlin_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_berlin_month 

write.csv2(avg_berlin_month,file ="../dataprep/avg_berlin_month.csv")

berlin_week$timestamp <- round_date(berlin_week$timestamp, unit ="hour")
berlin_week$timestamp
berlin_week$P2 <- as.numeric(as.character(berlin_week$P2))
tail(berlin_week)
avg_berlin_week <- berlin_week %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_berlin_week 
write.csv2(avg_berlin_week,file ="../dataprep/avg_berlin_week.csv")

berlin_days$timestamp <- round_date(berlin_days$timestamp, unit ="hour")
berlin_days$timestamp
berlin_days$P2 <- as.numeric(as.character(berlin_days$P2))
tail(berlin_days)
avg_berlin_days <- berlin_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_berlin_days 
write.csv2(avg_berlin_days,file ="../dataprep/avg_berlin_days.csv")




#cologne

cologne_month <- readr::read_csv2_chunked("../dataprep/raw_cologne_month.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
cologne_week <- readr::read_csv2_chunked("../dataprep/raw_cologne_week.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
cologne_days <- readr::read_csv2_chunked("../dataprep/raw_cologne_days.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)

cologne_month$timestamp <- round_date(cologne_month$timestamp, unit ="hour")
cologne_month$timestamp
cologne_month$P2 <- as.numeric(as.character(cologne_month$P2))
tail(cologne_month)
avg_cologne_month <- cologne_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_cologne_month 
write.csv2(avg_cologne_month,file ="../dataprep/avg_cologne_month.csv")

cologne_week$timestamp <- round_date(cologne_week$timestamp, unit ="hour")
cologne_week$timestamp
cologne_week$P2 <- as.numeric(as.character(cologne_week$P2))
tail(cologne_week)
avg_cologne_week <- cologne_week %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_cologne_week 
write.csv2(avg_cologne_week,file ="../dataprep/avg_cologne_week.csv")

cologne_days$timestamp <- round_date(cologne_days$timestamp, unit ="hour")
cologne_days$timestamp
cologne_days$P2 <- as.numeric(as.character(cologne_days$P2))
tail(cologne_days)
avg_cologne_days <- cologne_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_cologne_days 
write.csv2(avg_cologne_days,file ="../dataprep/avg_cologne_days.csv")


#frankfurt

frankfurt_month <- readr::read_csv2_chunked("../dataprep/raw_frankfurt_month.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
frankfurt_week <- readr::read_csv2_chunked("../dataprep/raw_frankfurt_week.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
frankfurt_days <- readr::read_csv2_chunked("../dataprep/raw_frankfurt_days.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)

frankfurt_month$timestamp <- round_date(frankfurt_month$timestamp, unit ="hour")
frankfurt_month$timestamp
frankfurt_month$P2 <- as.numeric(as.character(frankfurt_month$P2))
tail(frankfurt_month)
avg_frankfurt_month <- frankfurt_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_frankfurt_month 
write.csv2(avg_frankfurt_month,file ="../dataprep/avg_frankfurt_month.csv")

frankfurt_week$timestamp <- round_date(frankfurt_week$timestamp, unit ="hour")
frankfurt_week$timestamp
frankfurt_week$P2 <- as.numeric(as.character(frankfurt_week$P2))
tail(frankfurt_week)
avg_frankfurt_week <- frankfurt_week %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_frankfurt_week 
write.csv2(avg_frankfurt_week,file ="../dataprep/avg_frankfurt_week.csv")

frankfurt_days$timestamp <- round_date(frankfurt_days$timestamp, unit ="hour")
frankfurt_days$timestamp
frankfurt_days$P2 <- as.numeric(as.character(frankfurt_days$P2))
tail(frankfurt_days)
avg_frankfurt_days <- frankfurt_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_frankfurt_days 
write.csv2(avg_frankfurt_days,file ="../dataprep/avg_frankfurt_days.csv")

#hamburg

hamburg_month <- readr::read_csv2_chunked("../dataprep/raw_hamburg_month.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
hamburg_week <- readr::read_csv2_chunked("../dataprep/raw_hamburg_week.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
hamburg_days <- readr::read_csv2_chunked("../dataprep/raw_hamburg_days.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
hamburg_days

hamburg_month$timestamp <- round_date(hamburg_month$timestamp, unit ="hour")
hamburg_month$timestamp
hamburg_month$P2 <- as.numeric(as.character(hamburg_month$P2))
tail(hamburg_month)
avg_hamburg_month <- hamburg_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_hamburg_month 
write.csv2(avg_hamburg_month,file ="../dataprep/avg_hamburg_month.csv")

hamburg_week$timestamp <- round_date(hamburg_week$timestamp, unit ="hour")
hamburg_week$timestamp
hamburg_week$P2 <- as.numeric(as.character(hamburg_week$P2))
tail(hamburg_week)
avg_hamburg_week <- hamburg_week %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_hamburg_week 
write.csv2(avg_hamburg_week,file ="../dataprep/avg_hamburg_week.csv")

#workaround
days$timestamp <- round_date(days$timestamp, unit ="hour")
avg_hamburg_days <- days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
head(avg_hamburg_days)
tail(avg_hamburg_days)
write.csv2(avg_hamburg_days,file ="../dataprep/avg_hamburg_days.csv")
#

hamburg_days$timestamp <- round_date(hamburg_days$timestamp, unit ="hour")
hamburg_days$timestamp
hamburg_days$P2 <- as.numeric(as.character(hamburg_days$P2))
tail(hamburg_days)
avg_hamburg_days <- hamburg_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
tail(avg_hamburg_days) 
write.csv2(avg_hamburg_days,file ="../dataprep/avg_hamburg_days.csv")


#munich

munich_month <- readr::read_csv2_chunked("../dataprep/raw_munich_month.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
munich_week <- readr::read_csv2_chunked("../dataprep/raw_munich_week.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)
munich_days <- readr::read_csv2_chunked("../dataprep/raw_munich_days.csv", DataFrameCallback$new(f), chunk_size = 100000, col_names = TRUE)

munich_month$timestamp <- round_date(munich_month$timestamp, unit ="hour")
munich_month$timestamp
munich_month$P2 <- as.numeric(as.character(munich_month$P2))
tail(munich_month)
avg_munich_month <- munich_month%>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_munich_month 
write.csv2(avg_munich_month,file ="../dataprep/avg_munich_month.csv")

munich_week$timestamp <- round_date(munich_week$timestamp, unit ="hour")
munich_week$timestamp
munich_week$P2 <- as.numeric(as.character(munich_week$P2))
tail(munich_week)
avg_munich_week <- munich_week %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_munich_week 
write.csv2(avg_munich_week,file ="../dataprep/avg_munich_week.csv")

munich_days$timestamp <- round_date(munich_days$timestamp, unit ="hour")
munich_days$timestamp
munich_days$P2 <- as.numeric(as.character(munich_days$P2))
tail(munich_days)
avg_munich_days <- munich_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
avg_munich_days 
write.csv2(avg_munich_days,file ="../dataprep/avg_munich_days.csv")

avg_much <- munich_days %>% group_by(timestamp) %>% summarise(P2 = mean(P2))

#average
#bind months, days, weeks. then sort by date, group and calculate mean, then write

avg_cities_days <- bind_rows(avg_berlin_days,avg_cologne_days,avg_frankfurt_days,avg_hamburg_days,avg_munich_days)
head(avg_cities_days)
avg_cities_days
avg_cities_days <- avg_cities_days %>% arrange(avg_cities_days$timestamp) %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
tail(avg_cities_days)
write.csv2(avg_cities_days,file ="../dataprep/avg_cities_days.csv")

avg_cities_week <- bind_rows(avg_berlin_week,avg_cologne_week,avg_frankfurt_week,avg_hamburg_week,avg_munich_week)
avg_cities_week
avg_cities_week <- avg_cities_week %>% arrange(avg_cities_week$timestamp) %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
tail(avg_cities_week)
write.csv2(avg_cities_week,file ="../dataprep/avg_cities_week.csv")

avg_cities_month <- bind_rows(avg_berlin_month,avg_cologne_month,avg_frankfurt_month,avg_hamburg_month,avg_munich_month)
avg_cities_month
avg_cities_month <- avg_cities_month %>% arrange(avg_cities_month$timestamp) %>% group_by(timestamp) %>% summarise(P2 = mean(P2))
tail(avg_cities_month)
write.csv2(avg_cities_month,file ="../dataprep/avg_cities_month.csv")


#yexample <- readr::read_csv2_chunked()

#wrong statement
#f <- function(x, pos) subset(x, between(hp, 95, 100))
#mcars3 <- read_csv_chunked(readr_example("mtcars.csv"), DataFrameCallback$new(f), chunk_size = 7)
#mcars3

##unique sensors
#arrange nach ID,  group by ID, summarise (Id, lat, lon)

#berlin
berlin_sensors_october <- berlin_days %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(berlin_days$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
berlin_sensors_october <- unique(berlin_sensors_october, incomparables = FALSE)

write.csv2(berlin_sensors_october,file ="../dataprep/berlin_sensors_october.csv")

#cologne
cologne_sensors_october <- cologne_days %>% arrange(sensor_id) %>% group_by(sensor_id) %>% select(sensor_id, lat, lon) %>% unique( incomparables = FALSE)
cologne_sensors_october

write.csv2(cologne_sensors_october,file ="../dataprep/cologne_sensors_october.csv")

#frankfurt
frankfurt_sensors_october <- frankfurt_days %>% arrange(sensor_id) %>% group_by(sensor_id) %>% select(sensor_id, lat, lon) %>% unique( incomparables = FALSE)
frankfurt_sensors_october

write.csv2(frankfurt_sensors_october,file ="../dataprep/frankfurt_sensors_october.csv")

#hamburg
hamburg_sensors_october <- hamburg_days %>% arrange(sensor_id) %>% group_by(sensor_id) %>% select(sensor_id, lat, lon) %>% unique( incomparables = FALSE)
hamburg_sensors_october

write.csv2(hamburg_sensors_october,file ="../dataprep/hamburg_sensors_october.csv")



#munich
munich_sensors_october <- munich_days %>% arrange(sensor_id) %>% group_by(sensor_id) %>% select(sensor_id, lat, lon) %>% unique( incomparables = FALSE)
munich_sensors_october

write.csv2(munich_sensors_october,file ="../dataprep/munich_sensors_october.csv")



#control data
head(avg_berlin_days)
tail(avg_berlin_days)

head(avg_frankfurt_days)
tail(avg_frankfurt_days)

head(avg_hamburg_days)
tail(avg_hamburg_days)

head(avg_munich_days)
tail(avg_munich_days)


head(avg_berlin_week)
tail(avg_berlin_week)

head(avg_frankfurt_week)
tail(avg_frankfurt_week)

head(avg_hamburg_week)
tail(avg_hamburg_week)

head(avg_munich_week)
tail(avg_munich_week)


head(avg_berlin_month)
tail(avg_berlin_month)

head(avg_frankfurt_month)
tail(avg_frankfurt_month)

head(avg_hamburg_month)
tail(avg_hamburg_month)

head(avg_munich_month)
tail(avg_munich_month)


head(avg_cities_days)
tail(avg_cities_days)

head(avg_cities_week)
tail(avg_cities_week)

head(avg_cities_month)
tail(avg_cities_month)


#trying out stuff
nrow(filter(avg_munich_month, P2 > 30))

cologne_sensors_october
tail(cologne_sensors_october)
n(cologne_sensors_october)
n_distinct(cologne_sensors_october$sensor_id)
n_distinct(cologne_sensors_october)


######################################################################
######################################################################
######################################################################
 

