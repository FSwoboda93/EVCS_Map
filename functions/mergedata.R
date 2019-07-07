#file fro group5: Lufdaten
#merge together datasets from months to year in order to gain year data for dashboard

#read in data
berlin1 <- read.csv2("datafiles/Luftdaten/year/berlin1.csv")
berlin2 <- read.csv2("datafiles/Luftdaten/year/berlin2.csv")
berlin3 <- read.csv2("datafiles/Luftdaten/year/berlin3.csv")
berlin4 <- read.csv2("datafiles/Luftdaten/year/berlin4.csv")
berlin5 <- read.csv2("datafiles/Luftdaten/year/berlin5.csv")
berlin6 <- read.csv2("datafiles/Luftdaten/year/berlin6.csv")
berlin7 <- read.csv2("datafiles/Luftdaten/year/berlin7.csv")
berlin8 <- read.csv2("datafiles/Luftdaten/year/berlin8.csv")
berlin9 <- read.csv2("datafiles/Luftdaten/year/berlin9.csv")
berlin10 <- read.csv2("datafiles/Luftdaten/year/berlin10.csv")
berlin11 <- read.csv2("datafiles/Luftdaten/year/berlin11.csv")
berlin12 <- read.csv2("datafiles/Luftdaten/year/berlin12.csv")

#check if data of next month is within the data from each month.
tail(berlin12)

#manually delete last row when it shows data for next month
#(happens when data occurs like 1.6. 00:00 in month file of may)

berlin1 <- berlin1[-nrow(berlin1),]
berlin2 <- berlin2[-nrow(berlin2),]
berlin4 <- berlin4[-nrow(berlin4),]
berlin5 <- berlin5[-nrow(berlin5),]
berlin6 <- berlin6[-nrow(berlin6),]
berlin7 <- berlin7[-nrow(berlin7),]
berlin8 <- berlin8[-nrow(berlin8),]
berlin9 <- berlin9[-nrow(berlin9),]
berlin10<- berlin10[-nrow(berlin10),]
berlin11 <- berlin11[-nrow(berlin11),]
berlin12 <- berlin12[-nrow(berlin12),]


frankfurt1 <- read.csv2("datafiles/Luftdaten/year/frankfurt1.csv")
frankfurt2 <- read.csv2("datafiles/Luftdaten/year/frankfurt2.csv")
frankfurt3 <- read.csv2("datafiles/Luftdaten/year/frankfurt3.csv")
frankfurt4 <- read.csv2("datafiles/Luftdaten/year/frankfurt4.csv")
frankfurt5 <- read.csv2("datafiles/Luftdaten/year/frankfurt5.csv")
frankfurt6 <- read.csv2("datafiles/Luftdaten/year/frankfurt6.csv")
frankfurt7 <- read.csv2("datafiles/Luftdaten/year/frankfurt7.csv")
frankfurt8 <- read.csv2("datafiles/Luftdaten/year/frankfurt8.csv")
frankfurt9 <- read.csv2("datafiles/Luftdaten/year/frankfurt9.csv")
frankfurt10 <- read.csv2("datafiles/Luftdaten/year/frankfurt10.csv")
frankfurt11 <- read.csv2("datafiles/Luftdaten/year/frankfurt11.csv")
frankfurt12 <- read.csv2("datafiles/Luftdaten/year/frankfurt12.csv")

#no data from frankfurt from 31.3.2018
tail(frankfurt12)
frankfurt1 <- frankfurt1[-nrow(frankfurt1),]
frankfurt2 <- frankfurt2[-nrow(frankfurt2),]
frankfurt4 <- frankfurt4[-nrow(frankfurt4),]
frankfurt5 <- frankfurt5[-nrow(frankfurt5),]
frankfurt6 <- frankfurt6[-nrow(frankfurt6),]
frankfurt7 <- frankfurt7[-nrow(frankfurt7),]
frankfurt8 <- frankfurt8[-nrow(frankfurt8),]
frankfurt9 <- frankfurt9[-nrow(frankfurt9),]
frankfurt10<- frankfurt10[-nrow(frankfurt10),]
frankfurt11 <- frankfurt11[-nrow(frankfurt11),]
frankfurt12 <- frankfurt12[-nrow(frankfurt12),]


hamburg1 <- read.csv2("datafiles/Luftdaten/year/hamburg1.csv")
hamburg2 <- read.csv2("datafiles/Luftdaten/year/hamburg2.csv")
hamburg3 <- read.csv2("datafiles/Luftdaten/year/hamburg3.csv")
hamburg4 <- read.csv2("datafiles/Luftdaten/year/hamburg4.csv")
hamburg5 <- read.csv2("datafiles/Luftdaten/year/hamburg5.csv")
hamburg6 <- read.csv2("datafiles/Luftdaten/year/hamburg6.csv")
hamburg7 <- read.csv2("datafiles/Luftdaten/year/hamburg7.csv")
hamburg8 <- read.csv2("datafiles/Luftdaten/year/hamburg8.csv")
hamburg9 <- read.csv2("datafiles/Luftdaten/year/hamburg9.csv")
hamburg10 <- read.csv2("datafiles/Luftdaten/year/hamburg10.csv")
hamburg11 <- read.csv2("datafiles/Luftdaten/year/hamburg11.csv")
hamburg12 <- read.csv2("datafiles/Luftdaten/year/hamburg12.csv")

tail(hamburg12)
hamburg1 <- hamburg1[-nrow(hamburg1),]
hamburg2 <- hamburg2[-nrow(hamburg2),]
hamburg4 <- hamburg4[-nrow(hamburg4),]
hamburg5 <- hamburg5[-nrow(hamburg5),]
hamburg6 <- hamburg6[-nrow(hamburg6),]
hamburg7 <- hamburg7[-nrow(hamburg7),]
hamburg8 <- hamburg8[-nrow(hamburg8),]
hamburg9 <- hamburg9[-nrow(hamburg9),]
hamburg10<- hamburg10[-nrow(hamburg10),]
hamburg11 <- hamburg11[-nrow(hamburg11),]
hamburg12 <- hamburg12[-nrow(hamburg12),]



cologne1 <- read.csv2("datafiles/Luftdaten/year/cologne1.csv")
cologne2 <- read.csv2("datafiles/Luftdaten/year/cologne2.csv")
cologne3 <- read.csv2("datafiles/Luftdaten/year/cologne3.csv")
cologne4 <- read.csv2("datafiles/Luftdaten/year/cologne4.csv")
cologne5 <- read.csv2("datafiles/Luftdaten/year/cologne5.csv")
cologne6 <- read.csv2("datafiles/Luftdaten/year/cologne6.csv")
cologne7 <- read.csv2("datafiles/Luftdaten/year/cologne7.csv")
cologne8 <- read.csv2("datafiles/Luftdaten/year/cologne8.csv")
cologne9 <- read.csv2("datafiles/Luftdaten/year/cologne9.csv")
cologne10 <- read.csv2("datafiles/Luftdaten/year/cologne10.csv")
cologne11 <- read.csv2("datafiles/Luftdaten/year/cologne11.csv")
cologne12 <- read.csv2("datafiles/Luftdaten/year/cologne12.csv")

tail(cologne12)

cologne1 <- cologne1[-nrow(cologne1),]
cologne2 <- cologne2[-nrow(cologne2),]
cologne4 <- cologne4[-nrow(cologne4),]
cologne5 <- cologne5[-nrow(cologne5),]
cologne6 <- cologne6[-nrow(cologne6),]
cologne7 <- cologne7[-nrow(cologne7),]
cologne8 <- cologne8[-nrow(cologne8),]
cologne9 <- cologne9[-nrow(cologne9),]
cologne10<- cologne10[-nrow(cologne10),]
cologne11 <- cologne11[-nrow(cologne11),]
cologne12 <- cologne12[-nrow(cologne12),]


munich1 <- read.csv2("datafiles/Luftdaten/year/munich1.csv")
munich2 <- read.csv2("datafiles/Luftdaten/year/munich2.csv")
munich3 <- read.csv2("datafiles/Luftdaten/year/munich3.csv")
munich4 <- read.csv2("datafiles/Luftdaten/year/munich4.csv")
munich5 <- read.csv2("datafiles/Luftdaten/year/munich5.csv")
munich6 <- read.csv2("datafiles/Luftdaten/year/munich6.csv")
munich7 <- read.csv2("datafiles/Luftdaten/year/munich7.csv")
munich8 <- read.csv2("datafiles/Luftdaten/year/munich8.csv")
munich9 <- read.csv2("datafiles/Luftdaten/year/munich9.csv")
munich10 <- read.csv2("datafiles/Luftdaten/year/munich10.csv")
munich11 <- read.csv2("datafiles/Luftdaten/year/munich11.csv")
munich12 <- read.csv2("datafiles/Luftdaten/year/munich12.csv")

tail(munich9)

munich1 <- munich1[-nrow(munich1),]
munich2 <- munich2[-nrow(munich2),]
munich4 <- munich4[-nrow(munich4),]
munich5 <- munich5[-nrow(munich5),]
munich6 <- munich6[-nrow(munich6),]
munich7 <- munich7[-nrow(munich7),]
munich8 <- munich8[-nrow(munich8),]
munich9 <- munich9[-nrow(munich9),]
munich10<- munich10[-nrow(munich10),]
munich11 <- munich11[-nrow(munich11),]
munich12 <- munich12[-nrow(munich12),]


#cities describes the average of the 5 cities

cities1 <- read.csv2("datafiles/Luftdaten/year/cities1.csv")
cities2 <- read.csv2("datafiles/Luftdaten/year/cities2.csv")
cities3 <- read.csv2("datafiles/Luftdaten/year/cities3.csv")
cities4 <- read.csv2("datafiles/Luftdaten/year/cities4.csv")
cities5 <- read.csv2("datafiles/Luftdaten/year/cities5.csv")
cities6 <- read.csv2("datafiles/Luftdaten/year/cities6.csv")
cities7 <- read.csv2("datafiles/Luftdaten/year/cities7.csv")
cities8 <- read.csv2("datafiles/Luftdaten/year/cities8.csv")
cities9 <- read.csv2("datafiles/Luftdaten/year/cities9.csv")
cities10 <- read.csv2("datafiles/Luftdaten/year/cities10.csv")
cities11 <- read.csv2("datafiles/Luftdaten/year/cities11.csv")
cities12 <- read.csv2("datafiles/Luftdaten/year/cities12.csv")

# also exclude the mediocre value for 31.3. from data of mean of all cities since this one is not representative
#(data of only 2 cities compared to 364 other measurements of a year -> delete measurement)
tail(cities12)
cities1 <- cities1[-nrow(cities1),]
cities2 <- cities2[-nrow(cities2),]
cities3 <- cities3[-nrow(cities3),]
cities4 <- cities4[-nrow(cities4),]
cities5 <- cities5[-nrow(cities5),]
cities6 <- cities6[-nrow(cities6),]
cities7 <- cities7[-nrow(cities7),]
cities8 <- cities8[-nrow(cities8),]
cities9 <- cities9[-nrow(cities9),]
cities10<- cities10[-nrow(cities10),]
cities11 <- cities11[-nrow(cities11),]
cities12 <- cities12[-nrow(cities12),]

#also exclude 31.3. 
#-> no data from luftdaten provided from this day! see: https://archive.luftdaten.info/2018-03-31/
#-> this means data is only from one measurement at 0:00 -> exclude
#data from 31.3 for cities of: Berlin, Hamburg
#since this is the last day of march, we can just omit the last row of month march.

berlin3 <- berlin3[-nrow(berlin3),]
hamburg3 <- hamburg3[-nrow(hamburg3),]

#merge data together

data_year_berlin <- bind_rows(berlin1, berlin2, berlin3, berlin4, berlin5, berlin6,
                              berlin7, berlin8, berlin9, berlin10, berlin11, berlin12 )
data_year_berlin
write.csv2(data_year_berlin, "datafiles/Luftdaten/avg/avg_berlin_year.csv")


data_year_frankfurt <- bind_rows(frankfurt1, frankfurt2, frankfurt3, frankfurt4, frankfurt5, frankfurt6,
                              frankfurt7, frankfurt8, frankfurt9, frankfurt10, frankfurt11, frankfurt12 )
data_year_frankfurt
write.csv2(data_year_frankfurt, "datafiles/Luftdaten/avg/avg_frankfurt_year.csv")

data_year_hamburg <- bind_rows(hamburg1, hamburg2, hamburg3, hamburg4, hamburg5, hamburg6,
                              hamburg7, hamburg8, hamburg9, hamburg10, hamburg11, hamburg12 )
data_year_hamburg
write.csv2(data_year_hamburg, "datafiles/Luftdaten/avg/avg_hamburg_year.csv")

data_year_cologne <- bind_rows(cologne1, cologne2, cologne3, cologne4, cologne5, cologne6,
                              cologne7, cologne8, cologne9, cologne10, cologne11, cologne12 )
data_year_cologne
write.csv2(data_year_cologne, "datafiles/Luftdaten/avg/avg_cologne_year.csv")

data_year_munich <- bind_rows(munich1, munich2, munich3, munich4, munich5, munich6,
                              munich7, munich8, munich9, munich10, munich11, munich12 )
data_year_munich
write.csv2(data_year_munich, "datafiles/Luftdaten/avg/avg_munich_year.csv")

data_year_cities <- bind_rows(cities1, cities2, cities3, cities4, cities5, cities6,
                              cities7, cities8, cities9, cities10, cities11, cities12 )
data_year_cities
write.csv2(data_year_cities, "datafiles/Luftdaten/avg/avg_cities_year.csv")
