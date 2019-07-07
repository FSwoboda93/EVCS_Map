## data preparation for BI project: group 5 Luftdaten
## this file is filtering,selecting and preparing data
## to generate small data samples that can be used
## for plotting graphs in the dashboard

library(dplyr)
library(lubridate)
library(data.table)


## read files, add row descriptions optionally if required.
## the input data is all data from 1 sensor type of 1 month(here oct 2018)
## workaorund required for later work- maximum file size 5MB in RStudio -> readr

# you can find data here: https://archive.luftdaten.info/csv_per_month/2018-10/

data_pms7 <- read.csv2("../dataprep/2018-10_pms7003.csv")
#data_pms3 <- read.csv2("../dataprep/2018-10_pms5003.csv")
#data_ppd <- read.csv2("../dataprep/2018-10_ppd42ns.csv")

head(data_pms7)
summary(data_pms7)
#tail(data_pms7)
#str(data_pms7)

##we decided to focus on pms7003 sensor data at first: decent number of fitting sensors + data


##prepare data(no mising values/outliers)

data_pms7$sensor_id <- as.factor(data_pms7$sensor_id)
data_pms7$timestamp<- ymd_hms(data_pms7$timestamp)
data_pms7$P1 <- as.numeric(as.character(data_pms7$P1))
data_pms7$P2 <- as.numeric(as.character(data_pms7$P2))
data_pms7$P0 <- as.numeric(as.character(data_pms7$P0))
head(data_pms7)
summary(data_pms7$sensor_id, maxsum = 100)

#Ids are 8761 11813 12246 13030 14880 16397 16632 -> 5 required(1 for each city)
sensors <- c("8761","13030","14880","16632","16397" )
# soon: selection of sensors by lat/lon
#further note: time doesnt matter so far(Andreas), so we will choose the timeframe from 1.10.18 to 8.10.18(mday)

#filter(data_pms7, sensor_id ==14880)


## generate dataframe for a day/week for 1 sensor
daydata <- filter(data_pms7, mday(data_pms7$timestamp) ==1 & sensor_id ==14880)
#daydata
weekdata <- filter(data_pms7, mday(data_pms7$timestamp) %in%1:8 & sensor_id ==14880)
#weekdata

##write dataframe of a specific day/week
write.csv2(daydata,file ="../dataprep/sample_day.csv")
write.csv2(weekdata,file ="../dataprep/sample_week.csv")

##generate whole relevant dataframe -> select only relevant rows

relevant_data <- data_pms7 %>% select( sensor_id, timestamp, P1, P2) %>% filter(sensor_id %in% sensors &mday(timestamp)%in% 1:8)
summary(relevant_data)

day8761 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="8761")
day8761
week8761 <- filter(relevant_data, sensor_id =="8761")

day13030 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="13030")
day13030
week13030 <- filter(relevant_data, sensor_id =="13030")

day16632 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="16632")
week16632 <- filter(relevant_data, sensor_id =="16632")

day16397 <- filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="16397")
week16397 <- filter(relevant_data, sensor_id =="16397")

day14880 <-filter(relevant_data, mday(relevant_data$timestamp) ==1 & sensor_id =="14880")
week14880 <- filter(relevant_data, sensor_id =="14880")
  
day14880 <- create_avg_data(day14880,4)
week14880 <- create_avg_data(week14880,9)

write.csv2(day14880,file ="../dataprep/day14880.csv")
write.csv2(week14880,file ="../dataprep/week14880.csv")


day13030 <- create_avg_data(day13030,5)
week13030 <- create_avg_data(week13030,12)

write.csv2(day8761,file ="../dataprep/day8761.csv")
write.csv2(week8761,file ="../dataprep/week8761.csv")
write.csv2(day13030,file ="../dataprep/day13030.csv")
write.csv2(week13030,file ="../dataprep/week13030.csv")
write.csv2(day16632,file ="../dataprep/day16632.csv")
write.csv2(week16632,file ="../dataprep/week16632.csv")
write.csv2(day16397,file ="../dataprep/day16397.csv")
write.csv2(week16397,file ="../dataprep/week16397.csv")

write.csv2(relevant_data,file ="../dataprep/wholedata.csv")
#calculate average for every 10min/15 min/30min/hour
#also use this function for total average(relevant_data)

#relevant_data[,list(avg=mean(relevant_data$P2), mday(data_pms7$timestamp))]
summary(relevant_data)

# or just trim data by mean of every n rows

small_data <- head(daydata)
small_data
month(small_data$timestamp)
minute(small_data$timestamp)
second(small_data$timestamp)

#n()
#x <- sample(1:10, 1e5, rep = TRUE)
#length(unique(x))
#n_distinct(small_data)

mean(small_data$P1)
summarise(small_data, sensor_id, meanP1= mean(P1), mean_timestamp= mean(timestamp))

summarise(small_data, avg= mean(timestamp))

#calc_avg <- function(data_frame, n){
#  
#  counter <- n()
#  
#  while(<condition>){<instructions>}
#}
small_data
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
trim=2
small_data[which(1:nrow(small_data) %% trim == 1) , ]
n=2
small_data[1:(nrow(small_data)/n),]
mean(small_data$P1, by=group )
#summarise(small_data, avg=mean(P1), by=group)










#num_first <-1
#step <- 2
#numrow <- 3
#avg_date <- ymd_hms(c())
#avg_p1 <- c()
#nrow(small_data)
#for(i in 1:nrow(small_data))

create_avg_data <- function(db, step=2){
  
  num_first <-1
  numrow <- 1+ step
  avg_date <- ymd_hms(c(), tz = "CET")
  avg_p1 <- c()
  new_data <-data.frame("avg_timestamp" =avg_date, "avg_p1" =avg_p1)
  
  #nrow(data)
  
  while(numrow <=nrow(db)){
    
    #print(mean(data[num_first:numrow,]$P1))
    #print(mean(data[num_first:numrow,]$timestamp))
    #db <- day8761
    avg_p1 <-  mean(db[num_first:numrow,]$P1)
    avg_date <- mean(db[num_first:numrow,]$timestamp)
    nrow <- data.frame(avg_date, avg_p1)
    new_data <- rbind(new_data, nrow)
    
    
    num_first <- num_first + step+1
    numrow <- numrow + step+1
    
  }
  print(new_data)
  #avg_p1
  return (new_data)
  #+ create dataset 
  #new_data <-data.frame("avg_timestamp" =avg_date, "avg_p1" =avg_p1)
  
  
  
  
}

avgday8761 <-create_avg_data(day8761)
avgday8761

avg_week <- create_avg_data(relevant_data,33)
avg_week
write.csv2(avg_week, file ="../dataprep/avg_week.csv")

relevant_day <- relevant_data%>% filter(mday(relevant_data$timestamp)==1)
relevant_day


relevant_day <- create_avg_data(relevant_day, 11)
write.csv2(relevant_day, file ="../dataprep/avg_day.csv" )

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

  
  
  







