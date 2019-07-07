# File for load and preprocess of the data
library(dplyr)
library(googlePolylines)
library(ggmap)


#helperfunction for converting factors into numerics
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


#Helperfunction for unname and round named numerics of a given bounding box
round2digits <- function(x){ round(x, digits = 2)}
round_unname<- function(x){ round(unname(x), digits = 2)}

#read dataset (local path of the dataset (2 GB))
dbdata <- read.csv2("C:/Users/Lukas/Desktop/projekt_processed_datasets/old/masterdata.csv", header = TRUE)


#exclude unimportant information + convert download-/uploadrate from byte/s to kbit/s
rawdata <- data.frame("latitude" = dbdata$gps_breite,
                      "longtitude" = dbdata$gps_laenge,
                      "Downloadrate" = dbdata$tprx/125,
                      "Uploadrate" = dbdata$tptx/125)

#remove NA values for downloadrate
rawdata <- rawdata[!(is.na(rawdata$Downloadrate)),]

#remove NA values for uploadrate
rawdata <- rawdata[!(is.na(rawdata$Uploadrate)),]

#convert coordinates in the rawdata from datatyp factor to numeric in order to do the filter functions
#+round coordinates with 2 digits to filter as in the bounding box.(compensated with a relativly large bbox (f = 0,2))
Prepared_dataset <- data.frame("latitude" = round2digits(as.numeric.factor(rawdata$latitude)),
                   "longtitude" = round2digits(as.numeric.factor(rawdata$longtitude)),
                   "Downloadrate" = rawdata$Downloadrate,
                   "Uploadrate" = rawdata$Uploadrate)

#Generate comparable dataset for each route divided up into subroutes with
#key points as stops from the ice route https://reiseauskunft.bahn.de/. 
#Then filter prepared_dataset with bounding boxes of subroutes.

#1 Berlin 
lats_ber_stut <- c(52.525,50.106, 48.783)
longs_ber_stut <- c(13.369, 8.662, 9.182)
Ber_stut <- data.frame(longs_ber_stut, lats_ber_stut)

#1.1. Berlin -> Stuttgart (Berlin -> Frankfurt -> Stuttgart)

#Luftlinie Berlin <-> Frankfurt
ber_ffm <- data.frame("long" = c(52.525, 50.106),
                      "lat" = c(13.369, 8.662))
#Bounding Box für Berlin <-> Frankfurt
bbox_ber_ffm <- ggmap::make_bbox(long, lat, data = ber_ffm, f = 0.2)
bbox_ber_ffm <- round_unname(bbox_ber_ffm)
#Filter Dataset für Berlin <-> Frankfurt
ber_ffm_data <- Prepared_dataset %>% filter(latitude %in% (bbox_ber_ffm[1]:bbox_ber_ffm[3]))
ber_ffm_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_ber_ffm[2]:bbox_ber_ffm[4]))


#Bounding Box für Stuttgart <-> Frankfurt
stut_ffm <- data.frame("long" = c(50.106, 48.783),
                       "lat" = c(8.662, 9.182))
bbox_stutt_ffm <- ggmap::make_bbox(long, lat, data = stut_ffm, f = 0.2)
bbox_stutt_ffm <- round_unname(bbox_stutt_ffm)
#Filter dataset
stut_ffm_data <- Prepared_dataset %>% filter(latitude %in% (bbox_stutt_ffm[1]:bbox_stutt_ffm[3]))
stut_ffm_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_stutt_ffm[2]:bbox_stutt_ffm[4]))

#Merge for route Berlin -> Stuttart
ber_stut_data <- rbind(stut_ffm_data, ber_ffm_data)

#1.2. Hamburg -> Berlin
hh_ber <- data.frame("long" = c(53.553, 52.525),
                     "lat" = c(10.008, 13.369))
bbox_hh_ber <- ggmap::make_bbox(long, lat, data = hh_ber, f = 0.2)
bbox_hh_ber <- round_unname(bbox_hh_ber)
#Filter dataset
hh_ber_data <- Prepared_dataset %>% filter(latitude %in% (bbox_hh_ber[1]:bbox_hh_ber[3]))
hh_ber_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_hh_ber[2]:bbox_hh_ber[4]))

#1.3. Berlin -> München (via Nürnberg)
#Berlin Nürnberg
ber_nbg <- data.frame("long" = c(49.446, 52.525),
                     "lat" = c(11.082, 13.369))
bbox_ber_nbg <- ggmap::make_bbox(long, lat, data = ber_nbg, f = 0.2)
bbox_ber_nbg <- round_unname(bbox_ber_nbg)
#Filter dataset
ber_nbg_data <- Prepared_dataset %>% filter(latitude %in% (bbox_ber_nbg[1]:bbox_ber_nbg[3]))
ber_nbg_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_ber_nbg[2]:bbox_ber_nbg[4]))

#Nürnberg München
nbg_munch <- data.frame("long" = c(49.446, 48.140),
                      "lat" = c(11.082, 11.560))
bbox_nbg_munch <- ggmap::make_bbox(long, lat, data = nbg_munch, f = 0.2)
bbox_nbg_munch <- round_unname(bbox_nbg_munch)
#Filter dataset
nbg_munch_data <- Prepared_dataset %>% filter(latitude %in% (bbox_nbg_munch[1]:bbox_nbg_munch[3]))
nbg_munch_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_nbg_munch[2]:bbox_nbg_munch[4]))

#merge
ber_munch_data <- rbind(ber_nbg_data, nbg_munch_data)

#1.4. Hamburg -> München (via Berlin)
hh_munch_data <- rbind(hh_ber_data, ber_munch_data)

#1.5. Hamburg -> Stuttgart (via Köln)
#Hamburg Köln
hh_koe <- data.frame("long" = c(53.553, 50.943),
                        "lat" = c(10.008, 6.959))
bbox_hh_koe <- ggmap::make_bbox(long, lat, data = hh_koe, f = 0.2)
bbox_hh_koe <- round_unname(bbox_hh_koe)
#Filter dataset
hh_koe_data <- Prepared_dataset %>% filter(latitude %in% (bbox_hh_koe[1]:bbox_hh_koe[3]))
hh_koe_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_hh_koe[2]:bbox_hh_koe[4]))

#Köln Stuttgart
koe_stutt <- data.frame("long" = c(50.943,50.106),
                     "lat" = c(6.959,8.662))
bbox_koe_stutt <- ggmap::make_bbox(long, lat, data = koe_stutt, f = 0.2)
bbox_koe_stutt <- round_unname(bbox_hh_koe)
#Filter dataset
koe_stutt_data <- Prepared_dataset %>% filter(latitude %in% (bbox_koe_stutt[1]:bbox_koe_stutt[3]))
koe_stutt_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_koe_stutt[2]:bbox_koe_stutt[4]))

#merge
hh_stutt_data <- rbind(hh_koe_data, koe_stutt_data)

#1.6. Hamburg -> Frankfurt (via Köln)

#Köln Frankfurt
koe_ffm <- data.frame("long" = c(50.943, 50.108),
                     "lat" = c(6.959, 8.665))
bbox_koe_ffm <- ggmap::make_bbox(long, lat, data = koe_ffm, f = 0.2)
bbox_koe_ffm <- round_unname(bbox_koe_ffm)
#Filter dataset
koe_ffm_data <- Prepared_dataset %>% filter(latitude %in% (bbox_koe_ffm[1]:bbox_koe_ffm[3]))
koe_ffm_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_koe_ffm[2]:bbox_koe_ffm[4]))


#merge
hh_ffm_data <- rbind(hh_koe_data, koe_ffm_data)

#1.7. München -> Stuttgart
munch_stut <- data.frame("long" = c(48.140, 48.784),
                        "lat" = c(11.560, 9.179))
bbox_munch_stut <- ggmap::make_bbox(long, lat, data = munch_stut, f = 0.2)
bbox_munch_stut <- round_unname(bbox_munch_stut)
#Filter dataset
munch_stut_data <- Prepared_dataset %>% filter(latitude %in% (bbox_munch_stut[1]:bbox_munch_stut[3]))
munch_stut_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_munch_stut[2]:bbox_munch_stut[4]))

#1.8. München -> Frankfurt
munch_ffm <- data.frame("long" = c(48.140, 50.107),
                         "lat" = c(11.560, 8.662))
bbox_munch_ffm <- ggmap::make_bbox(long, lat, data = munch_ffm, f = 0.2)
bbox_munch_ffm <- round_unname(bbox_munch_stut)
#Filter dataset
munch_ffm_data <- Prepared_dataset %>% filter(latitude %in% (bbox_munch_ffm[1]:bbox_munch_ffm[3]))
munch_ffm_data <- Prepared_dataset %>% filter(longtitude %in% (bbox_munch_ffm[2]:bbox_munch_ffm[4]))

#1.9. Berlin -> Köln
ber_koe_data <- rbind(hh_ber_data, hh_koe_data)


#1.9.2. München -> Köln
munch_koe_data <- rbind(munch_ffm_data, koe_ffm_data)

#2.Generate Datasets for the Plotelement

#Berlin BS; HB ; BM ; BF
Berlin <- data.frame("routes" = c("BR - HH", "BR - MC", 
                                  "BR - CL", "BR - FR" , "BR - HH", "BR - MC", "BR - CL", "BR - FR"),
                     "rate" = c("Downloadrate", "Downloadrate", "Downloadrate", "Downloadrate",
                                "Uploadrate", "Uploadrate", "Uploadrate", "Uploadrate"),
                     "meanDown" = round(mean(Prepared_dataset$Downloadrate),digits = 0),
                     "meanUp" = round(mean(Prepared_dataset$Uploadrate),digits = 0),
                     "mean" = c(round(mean(hh_ber_data$Downloadrate),digits = 0), round(mean(ber_munch_data$Downloadrate),digits = 0), round(mean(ber_koe_data$Downloadrate), digits = 0),
                                round(mean(ber_ffm_data$Downloadrate),digits = 0), round(mean(hh_ber_data$Uploadrate),digits=0), round(mean(ber_munch_data$Uploadrate),digits =0), round(mean(ber_koe_data$Uploadrate),digits=0),
                                round(mean(ber_ffm_data$Uploadrate),digits = 0) )
                     )
#Cologne BC HC MC CF
Cologne <- data.frame("routes" = c("CL - BR", "CL - MC", 
                                   "CL - HH", "CL - FR" , "CL - BR", "CL - MC", "CL - HH", "CL - FR"),
                      "rate" = c("Downloadrate", "Downloadrate", "Downloadrate", "Downloadrate",
                                 "Uploadrate", "Uploadrate", "Uploadrate", "Uploadrate"),
                      "meanDown" = round(mean(Prepared_dataset$Downloadrate),digits =0),
                      "meanUp" = round(mean(Prepared_dataset$Uploadrate),digits=0),
                      "mean" = c(round(mean(ber_koe_data$Downloadrate),digits = 0), round(mean(munch_koe_data$Downloadrate),digits = 0), round(mean(hh_koe_data$Downloadrate),digits = 0),
                                 round(mean(koe_ffm_data$Downloadrate),digits=0), round(mean(ber_koe_data$Uploadrate),digits=0), round(mean(munch_koe_data$Uploadrate),digits=0), round(mean(hh_koe_data$Uploadrate),digits=0),
                                 round(mean(koe_ffm_data$Uploadrate),digits=0) )
)

#Frankfurt HF BF MF CF
Frankfurt <- data.frame("routes" = c("FR - BR", "FR - MC", 
                                     "FR - CL", "FR - HH" , "FR - BR", "FR - MC", 
                                     "FR - CL", "FR - HH"),
                        "rate" = c("Downloadrate", "Downloadrate", "Downloadrate", "Downloadrate",
                                   "Uploadrate", "Uploadrate", "Uploadrate", "Uploadrate"),
                        "meanDown" = round(mean(Prepared_dataset$Downloadrate),digits=0),
                        "meanUp" = round(mean(Prepared_dataset$Uploadrate),digits=0),
                        "mean" = c(round(mean(ber_ffm_data$Downloadrate),digits=0), round(mean(munch_ffm_data$Downloadrate),digits=0), round(mean(koe_ffm_data$Downloadrate),digits = 0),
                                   round(mean(hh_ffm_data$Downloadrate),digits = 0), round(mean(ber_ffm_data$Uploadrate),digits =0 ), round(mean(munch_ffm_data$Uploadrate),digits=0), round(mean(koe_ffm_data$Uploadrate),digits=0),
                                   round(mean(hh_ffm_data$Uploadrate),digits =0) ))
#Hamburg HB HM HC HF
Hamburg <- data.frame("routes" = c("HH - BR", "HH - MC", 
                                   "HH - CL", "HH - FR" , "HH - BR", "HH - MC", 
                                   "HH - CL", "HH - FR"),
                      "rate" = c("Downloadrate", "Downloadrate", "Downloadrate", "Downloadrate",
                                 "Uploadrate", "Uploadrate", "Uploadrate", "Uploadrate"),
                      "meanDown" = round(mean(Prepared_dataset$Downloadrate),digits =0),
                      "meanUp" = round(mean(Prepared_dataset$Uploadrate),digits=0),
                      "mean" = c(round(mean(hh_ber_data$Downloadrate),digits=0), round(mean(hh_munch_data$Downloadrate),digits=0), round(mean(hh_koe_data$Downloadrate),digits=0),
                                 round(mean(hh_ffm_data$Downloadrate),digits=0), round(mean(hh_ber_data$Uploadrate),digits=0), round(mean(hh_munch_data$Uploadrate),digits =0), round(mean(hh_koe_data$Uploadrate),digits=0),
                                 round(mean(hh_ffm_data$Uploadrate),digits=0) )) 
#Munich HM BM MC MF
Munich <- data.frame("routes" = c("MC - BR", "MC - HH", 
                                  "MC - CL", "MC - FR" , "MC - BR", "MC - HH", 
                                  "MC - CL", "MC - FR"),
                     "rate" = c("Downloadrate", "Downloadrate", "Downloadrate", "Downloadrate",
                                "Uploadrate", "Uploadrate", "Uploadrate", "Uploadrate"),
                     "meanDown" = round(mean(Prepared_dataset$Downloadrate),digits =0),
                     "meanUp" = round(mean(Prepared_dataset$Uploadrate),digits =0),
                     "mean" = c(round(mean(ber_munch_data$Downloadrate),digits =0), round(mean(hh_munch_data$Downloadrate),digits=0), round(mean(munch_koe_data$Downloadrate),digits =0),
                                round(mean(munch_ffm_data$Downloadrate),digits =0), round(mean(ber_munch_data$Uploadrate),digits =0), round(mean(hh_munch_data$Uploadrate),digits =0), round(mean(munch_koe_data$Uploadrate),digits=0),
                                round(mean(munch_ffm_data$Uploadrate),digits=0) ))

#done
#write.csv(Munich, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Munich.csv", row.names = FALSE)
#write.csv(Hamburg, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Hamburg.csv", row.names = FALSE)
#write.csv(Cologne, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Cologne.csv", row.names = FALSE)
#write.csv(Frankfurt, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Frankfurt.csv", row.names = FALSE)
#write.csv(Berlin, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Berlin.csv", row.names = FALSE)

#MAPDATA
#Calculate the %  of the WLAN(Downloadrate) which is under 32 kbits/s for each Route + average Downloadrate
#1. Berlin
#Berlin - Frankfurt
numOfMeas_ber_ffm <- length(ber_ffm_data$Downloadrate)
low_ber_ffm <- length(which(ber_ffm_data$Downloadrate < 32))
#Stability in %
avg_ber_ffm <- round(mean(ber_ffm_data$Downloadrate),digits=0)
slow_ber_ffm <- 100 - round((numOfMeas_ber_ffm - low_ber_ffm)/(numOfMeas_ber_ffm/100),digits = 2)
#standartabweichung
sd_ber_ffm <- round(sd(ber_ffm_data$Downloadrate),digits=0)
save(sd_ber_ffm, file = "sd_ber_ffm.Rda")

save(avg_ber_ffm, file = "avg_ber_ffm.Rda")
save(slow_ber_ffm, file = "slow_ber_ffm.Rda")

#Berlin - Cologne
numOfMeas_ber_koe <- length(ber_koe_data$Downloadrate)
low_ber_koe <- length(which(ber_koe_data$Downloadrate < 32))
#Stability in %
avg_ber_koe <- round(mean(ber_koe_data$Downloadrate),digits = 0)
slow_ber_koe <- 100 - round((numOfMeas_ber_koe - low_ber_koe)/(numOfMeas_ber_koe/100),digits = 2)
#standartabweichung
sd_ber_koe <- round(sd(ber_koe_data$Downloadrate),digits=0)
save(sd_ber_koe, file = "sd_ber_koe.Rda")

save(avg_ber_koe, file = "avg_ber_koe.Rda")
save(slow_ber_koe, file = "slow_ber_koe.Rda")

#Berlin - Hamburg
numOfMeas_hh_ber <- length(hh_ber_data$Downloadrate)
low_hh_ber <- length(which(hh_ber_data$Downloadrate < 32))
#Stability in %
avg_hh_ber <- round(mean(hh_ber_data$Downloadrate),digits=0)
slow_hh_ber <- 100 - round((numOfMeas_hh_ber - low_hh_ber)/(numOfMeas_hh_ber/100),digits = 2)
#standartabweichung
sd_hh_ber <- round(sd(hh_ber_data$Downloadrate),digits=0)
save(sd_hh_ber, file = "sd_hh_ber.Rda")

save(avg_hh_ber, file = "avg_hh_ber.Rda")
save(slow_hh_ber, file = "slow_hh_ber.Rda")

#Berlin - München
numOfMeas_ber_munch <- length(ber_munch_data$Downloadrate)
low_ber_munch <- length(which(ber_munch_data$Downloadrate < 32))
#Stability in %
avg_ber_munch <- round(mean(ber_munch_data$Downloadrate),digits =0)
slow_ber_munch <- 100 - round((numOfMeas_ber_munch - low_ber_munch)/(numOfMeas_ber_munch/100),digits = 2)

#standartabweichung
sd_ber_munch <- round(sd(ber_munch_data$Downloadrate),digits=0)
save(sd_ber_munch, file = "sd_ber_munch.Rda")

save(avg_ber_munch, file = "avg_ber_munch.Rda")
save(slow_ber_munch, file = "slow_ber_munch.Rda")

#Hamburg - M?nchen
numOfMeas_hh_munch <- length(hh_munch_data$Downloadrate)
low_hh_munch <- length(which(hh_munch_data$Downloadrate < 32))
#Stability in %
avg_hh_munch <- round(mean(hh_munch_data$Downloadrate),digits =0)
slow_hh_munch <- 100 - round((numOfMeas_hh_munch - low_hh_munch)/(numOfMeas_hh_munch/100),digits = 2)
#standartabweichung
sd_hh_munch <- round(sd(hh_munch_data$Downloadrate),digits=0)
save(sd_hh_munch, file = "sd_hh_munch.Rda")

save(avg_hh_munch, file = "avg_hh_munch.Rda")
save(slow_hh_munch, file = "slow_hh_munch.Rda")

#Hamburg - Cologne
numOfMeas_hh_koe <- length(hh_koe_data$Downloadrate)
low_hh_koe <- length(which(hh_koe_data$Downloadrate < 32))
#Stability in %
avg_hh_koe <- round(mean(hh_koe_data$Downloadrate),digits =0)
slow_hh_koe <- 100 - round((numOfMeas_hh_koe - low_hh_koe)/(numOfMeas_hh_koe/100),digits = 2)
#standartabweichung
sd_hh_koe <- round(sd(hh_koe_data$Downloadrate),digits=0)
save(sd_hh_koe, file = "sd_hh_koe.Rda")

save(avg_hh_koe, file = "avg_hh_koe.Rda")
save(slow_hh_koe, file = "slow_hh_koe.Rda")

#Hamburg - Frankfurt
numOfMeas_hh_ffm <- length(hh_ffm_data$Downloadrate)
low_hh_ffm <- length(which(hh_ffm_data$Downloadrate < 32))
#Stability in %
avg_hh_ffm <- round(mean(hh_ffm_data$Downloadrate),digits =0)
slow_hh_ffm <- 100 - round((numOfMeas_hh_ffm - low_hh_ffm)/(numOfMeas_hh_ffm/100),digits = 2)
#standartabweichung
sd_hh_ffm <- round(sd(hh_ffm_data$Downloadrate),digits=0)
save(sd_hh_ffm, file = "sd_hh_ffm.Rda")

save(avg_hh_ffm, file = "avg_hh_ffm.Rda")
save(slow_hh_ffm, file = "slow_hh_ffm.Rda")

#M?nchen - Cologne
numOfMeas_munch_koe <- length(munch_koe_data$Downloadrate)
low_munch_koe <- length(which(munch_koe_data$Downloadrate < 32))
#Stability in %
avg_munch_koe <- round(mean(munch_koe_data$Downloadrate),digits =0)
slow_munch_koe <- 100 - round((numOfMeas_munch_koe - low_munch_koe)/(numOfMeas_munch_koe/100),digits = 2)
#standartabweichung
sd_munch_koe <- round(sd(munch_koe_data$Downloadrate),digits=0)
save(sd_munch_koe, file = "sd_munch_koe.Rda")

save(avg_munch_koe, file = "avg_munch_koe.Rda")
save(slow_munch_koe, file = "slow_munch_koe.Rda")

#M?nchen - Frankfurt
numOfMeas_munch_ffm <- length(munch_ffm_data$Downloadrate)
low_munch_ffm <- length(which(munch_ffm_data$Downloadrate < 32))
#Stability in %
avg_munch_ffm <- round(mean(munch_ffm_data$Downloadrate),digits =0)
slow_munch_ffm <- 100 - round((numOfMeas_munch_ffm - low_munch_ffm)/(numOfMeas_munch_ffm/100),digits = 2)
#standartabweichung
sd_munch_ffm <- round(sd(munch_ffm_data$Downloadrate),digits=0)
save(sd_munch_ffm, file = "sd_munch_ffm.Rda")

save(avg_munch_ffm, file = "avg_munch_ffm.Rda")
save(slow_munch_ffm, file = "slow_munch_ffm.Rda")


#Cologne - Frankfurt
numOfMeas_koe_ffm <- length(koe_ffm_data$Downloadrate)
low_koe_ffm <- length(which(koe_ffm_data$Downloadrate < 32))
#Stability in %
avg_koe_ffm <- round(mean(koe_ffm_data$Downloadrate),digits =0)
slow_koe_ffm <- 100 - round((numOfMeas_koe_ffm - low_koe_ffm)/(numOfMeas_koe_ffm/100),digits = 2)
#standartabweichung
sd_koe_ffm <- round(sd(koe_ffm_data$Downloadrate),digits=0)
save(sd_koe_ffm, file = "sd_koe_ffm.Rda")

save(avg_koe_ffm, file = "avg_koe_ffm.Rda")
save(slow_koe_ffm, file = "slow_koe_ffm.Rda")

#Dataprocessing for the map train route element (not implemented)
#make bounding boxes around each city and filter the dataset in order to display the rail routes from a certain location with less overhead.

#Berlin bbox. for drawing train routes. Not used because of overall performance issues

#Berlin_bb <- data.frame("long" = c(52.46, 52.57),
#                      "lat" = c(13.26, 13.52))
#Berlin_bbox <- ggmap::make_bbox(long, lat, data = Berlin_bb, f = 0.1)
#Berlin_bbox <- round_unname(Berlin_bbox)
#Filter dataset
#Berlin_bbox_data <- Prepared_dataset %>% filter(latitude %in% (Berlin_bbox[1]:Berlin_bbox[3]))
#Berlin_bbox_data <- Prepared_dataset %>% filter(longtitude %in% (Berlin_bbox[2]:Berlin_bbox[4]))
#Berlin_map_data <- data.frame("latitude" = Berlin_bbox_data$latitude,
#                               "longtitude" = Berlin_bbox_data$longtitude,
#                              "Downloadrate" = round(Berlin_bbox_data$Downloadrate, digits = 0))
                               
#write.csv(Berlin_map_data, file="C:/Users/Lukas/Desktop/R workspace/2018-ws-mobility-dashboard/data/Berlin_map_data.csv", row.names = FALSE)




