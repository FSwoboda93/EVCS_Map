#file from group5: Luftdaten
#merge together sensors from months in order to gain a year file of sensors for each city

#read in data
sensors_berlin1 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin1.csv")
sensors_berlin2 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin2.csv")
sensors_berlin3 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin3.csv")
sensors_berlin4 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin4.csv")
sensors_berlin5 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin5.csv")
sensors_berlin6 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin6.csv")
sensors_berlin7 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin7.csv")
sensors_berlin8 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin8.csv")
sensors_berlin9 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin9.csv")
sensors_berlin10 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin10.csv")
sensors_berlin11 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin11.csv")
sensors_berlin12 <- read.csv2("datafiles/Luftdaten/year/sensors_berlin12.csv")

#aggregate sensor data and write it in a file
sensors_berlin_combined <- bind_rows(sensors_berlin1, sensors_berlin2, sensors_berlin3, sensors_berlin4,
                                 sensors_berlin5, sensors_berlin6, sensors_berlin7, sensors_berlin8,
                                 sensors_berlin9, sensors_berlin10, sensors_berlin11, sensors_berlin12 )

sensors_berlin_year<- sensors_berlin_combined %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(sensors_berlin_combined$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
head(sensors_berlin_year)
sensors_berlin_year <- unique(sensors_berlin_year)
write.csv2(sensors_berlin_year, "datafiles/Luftdaten/sensors/berlin_sensors_year.csv")


sensors_frankfurt1 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt1.csv")
sensors_frankfurt2 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt2.csv")
sensors_frankfurt3 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt3.csv")
sensors_frankfurt4 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt4.csv")
sensors_frankfurt5 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt5.csv")
sensors_frankfurt6 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt6.csv")
sensors_frankfurt7 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt7.csv")
sensors_frankfurt8 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt8.csv")
sensors_frankfurt9 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt9.csv")
sensors_frankfurt10 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt10.csv")
sensors_frankfurt11 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt11.csv")
sensors_frankfurt12 <- read.csv2("datafiles/Luftdaten/year/sensors_frankfurt12.csv")


#aggregate sensor data and write it in a file
sensors_frankfurt_combined <- bind_rows(sensors_frankfurt1, sensors_frankfurt2, sensors_frankfurt3, sensors_frankfurt4,
                                     sensors_frankfurt5, sensors_frankfurt6, sensors_frankfurt7, sensors_frankfurt8,
                                     sensors_frankfurt9, sensors_frankfurt10, sensors_frankfurt11, sensors_frankfurt12 )

sensors_frankfurt_year<- sensors_frankfurt_combined %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(sensors_frankfurt_combined$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
head(sensors_frankfurt_year)
sensors_frankfurt_year <- unique(sensors_frankfurt_year)
write.csv2(sensors_frankfurt_year, "datafiles/Luftdaten/sensors/frankfurt_sensors_year.csv")


sensors_hamburg1 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg1.csv")
sensors_hamburg2 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg2.csv")
sensors_hamburg3 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg3.csv")
sensors_hamburg4 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg4.csv")
sensors_hamburg5 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg5.csv")
sensors_hamburg6 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg6.csv")
sensors_hamburg7 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg7.csv")
sensors_hamburg8 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg8.csv")
sensors_hamburg9 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg9.csv")
sensors_hamburg10 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg10.csv")
sensors_hamburg11 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg11.csv")
sensors_hamburg12 <- read.csv2("datafiles/Luftdaten/year/sensors_hamburg12.csv")


#aggregate sensor data and write it in a file
sensors_hamburg_combined <- bind_rows(sensors_hamburg1, sensors_hamburg2, sensors_hamburg3, sensors_hamburg4,
                                     sensors_hamburg5, sensors_hamburg6, sensors_hamburg7, sensors_hamburg8,
                                     sensors_hamburg9, sensors_hamburg10, sensors_hamburg11, sensors_hamburg12 )

sensors_hamburg_year<- sensors_hamburg_combined %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(sensors_hamburg_combined$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
head(sensors_hamburg_year)
sensors_hamburg_year <- unique(sensors_hamburg_year)
write.csv2(sensors_hamburg_year, "datafiles/Luftdaten/sensors/hamburg_sensors_year.csv")



sensors_cologne1 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne1.csv")
sensors_cologne2 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne2.csv")
sensors_cologne3 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne3.csv")
sensors_cologne4 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne4.csv")
sensors_cologne5 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne5.csv")
sensors_cologne6 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne6.csv")
sensors_cologne7 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne7.csv")
sensors_cologne8 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne8.csv")
sensors_cologne9 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne9.csv")
sensors_cologne10 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne10.csv")
sensors_cologne11 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne11.csv")
sensors_cologne12 <- read.csv2("datafiles/Luftdaten/year/sensors_cologne12.csv")


#aggregate sensor data and write it in a file
sensors_cologne_combined <- bind_rows(sensors_cologne1, sensors_cologne2, sensors_cologne3, sensors_cologne4,
                                     sensors_cologne5, sensors_cologne6, sensors_cologne7, sensors_cologne8,
                                     sensors_cologne9, sensors_cologne10, sensors_cologne11, sensors_cologne12 )

sensors_cologne_year<- sensors_cologne_combined %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(sensors_cologne_combined$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
head(sensors_cologne_year)
sensors_cologne_year <- unique(sensors_cologne_year)
write.csv2(sensors_cologne_year, "datafiles/Luftdaten/sensors/hamburg_sensors_year.csv")

sensors_munich1 <- read.csv2("datafiles/Luftdaten/year/sensors_munich1.csv")
sensors_munich2 <- read.csv2("datafiles/Luftdaten/year/sensors_munich2.csv")
sensors_munich3 <- read.csv2("datafiles/Luftdaten/year/sensors_munich3.csv")
sensors_munich4 <- read.csv2("datafiles/Luftdaten/year/sensors_munich4.csv")
sensors_munich5 <- read.csv2("datafiles/Luftdaten/year/sensors_munich5.csv")
sensors_munich6 <- read.csv2("datafiles/Luftdaten/year/sensors_munich6.csv")
sensors_munich7 <- read.csv2("datafiles/Luftdaten/year/sensors_munich7.csv")
sensors_munich8 <- read.csv2("datafiles/Luftdaten/year/sensors_munich8.csv")
sensors_munich9 <- read.csv2("datafiles/Luftdaten/year/sensors_munich9.csv")
sensors_munich10 <- read.csv2("datafiles/Luftdaten/year/sensors_munich10.csv")
sensors_munich11 <- read.csv2("datafiles/Luftdaten/year/sensors_munich11.csv")
sensors_munich12 <- read.csv2("datafiles/Luftdaten/year/sensors_munich12.csv")


#aggregate sensor data and write it in a file
sensors_munich_combined <- bind_rows(sensors_munich1, sensors_munich2, sensors_munich3, sensors_munich4,
                                     sensors_munich5, sensors_munich6, sensors_munich7, sensors_munich8,
                                     sensors_munich9, sensors_munich10, sensors_munich11, sensors_munich12 )

sensors_munich_year<- sensors_munich_combined %>% arrange(sensor_id) %>% group_by(sensor_id) %>% unique(sensors_munich_combined$sensor_id, incomparables = FALSE) %>%select(sensor_id, lat, lon)
head(sensors_munich_year)
sensors_munich_year <- unique(sensors_munich_year)
write.csv2(sensors_munich_year, "datafiles/Luftdaten/sensors/munich_sensors_year.csv")

