#Data variables, calculated in (../plotData6/dataprocessing/data_processing6.R)

#Durchschnittliche Downloadrate
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_ber_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_ber_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_ber_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_hh_ber.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_hh_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_hh_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_hh_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_koe_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_munch_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/avg_munch_koe.Rda")

# %teil der jeweiligen Strecke mit sehr schlechtem Internet
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_ber_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_ber_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_ber_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_hh_ber.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_hh_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_hh_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_hh_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_koe_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_munch_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/slow_munch_koe.Rda")

#Standartabweichung für die Downloadgeschwindig für die jeweiliuge Strecke
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_ber_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_ber_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_ber_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_hh_ber.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_hh_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_hh_koe.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_hh_munch.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_koe_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_munch_ffm.Rda")
load("../2018-ws-mobility-dashboard/datafiles/DBOpenData/sd_munch_koe.Rda")

berlin_trainstation <- function (global){
popupcontent_berlin <-  paste(sep = "<br/>",
                                "<b><a>Berlin Trainstation</a></b>",
                              "",
                              "Click the train route",
                              "to get WIFI Information")
return (popupcontent_berlin)
}
hamburg_trainstation <- function (global){
  popupcontent_hamburg <-  paste(sep = "<br/>",
                                "<b><a>Hamburg Trainstation</a></b>",
                                "",
                                "Click the train route",
                                "to get WIFI Information")
  return (popupcontent_hamburg)
}
cologne_trainstation <- function (global){
  popupcontent_Cologne <-  paste(sep = "<br/>",
                                 "<b><a>Cologne Trainstation</a></b>",
                                 "",
                                 "Click the train route",
                                 "to get WIFI Information")
  return (popupcontent_Cologne)
}
munich_trainstation <- function (global){
  popupcontent_Munich <-  paste(sep = "<br/>",
                                 "<b><a>Munich Trainstation</a></b>",
                                "",
                                "Click the train route",
                                "to get WIFI Information")
  return (popupcontent_Munich)
}
frankfurt_trainstation <- function (global){
  popupcontent_frankfurt <-  paste(sep = "<br/>",
                                 "<b><a>Frankfurt Trainstation</a></b>",
                                 "",
                                 "Click the train route",
                                 "to get WIFI Information")
  return (popupcontent_frankfurt)
}

#Trainroute mapdata for routes between trainstations
info_bermunch <- function (global){
  popupcontent_bermunch <-  paste(
                                  "<b><a>ICE Train route Berlin Munich</a></b></br>",
                                  "Average Downloadrate: ", avg_ber_munch," kbits/s </br>",
                                  "Standard deviation: ", sd_ber_munch," kbits/s </br>",
                                  round(slow_ber_munch,digits = 2) , "% of the route has less than 32 kbits/s")
  return (popupcontent_bermunch)
}
info_bercol <- function (global){
  popupcontent_bercol <-  paste("<b><a>ICE Train route Berlin Cologne</a></b></br>",
                                "Average Downloadrate: ", avg_ber_koe," kbits/s </br>",
                                "Standard deviation: ", sd_ber_koe," kbits/s </br>",
                                slow_ber_koe , "% of the route has less than 32 kbits/s")
  return (popupcontent_bercol)
}
info_berffm <- function (global){
  popupcontent_berffm <-  paste(
                                "<b><a>ICE Train route Berlin Frankfurt</a></b></br>",
                                "Average Downloadrate: ", avg_ber_ffm," kbits/s </br>",
                                "Standard deviation: ", sd_ber_ffm," kbits/s </br>",
                                slow_ber_ffm , "% of the route has less than 32 kbits/s")
  return (popupcontent_berffm)
}
info_berhh <- function (global){
  popupcontent_berhh <-  paste("<b><a>ICE Train route Berlin Hamburg</a></b></br>",
                               "Downloadrate: ", avg_hh_ber," kbits/s </br>",
                               "Standard deviation: ", sd_hh_ber," kbits/s </br>",
                               slow_hh_ber , "% of the route has less than 32 kbits/s")
  return (popupcontent_berhh)
}
info_hhmunch <- function (global){
  popupcontent_hhmunch <-  paste("<b><a>ICE Train route Hamburg Munich</a></b></br>",
                                 "Average Downloadrate: ", avg_hh_munch," kbits/s </br>",
                                 "Standard deviation: ", sd_hh_munch," kbits/s </br>",
                                 slow_hh_munch , "% of the route has less than 32 kbits/s")
  return (popupcontent_hhmunch)
}
info_hhcol <- function (global){
  popupcontent_hhcol <-  paste("<b><a>ICE Train route Hamburg Cologne</a></b></br>",
                               "Average Downloadrate: ", avg_hh_koe," kbits/s </br>",
                               "Standard deviation: ", sd_hh_koe," kbits/s </br>",
                               slow_hh_koe , "% of the route has less than 32 kbits/s")
  return (popupcontent_hhcol)
}
info_hhffm <- function (global){
  popupcontent_hhffm <-  paste("<b><a>ICE Train route Hamburg Frankfurt</a></b></br>",
                               "Downloadrate: ", avg_hh_ffm," kbits/s </br>",
                               "Standard deviation: ", sd_hh_ffm," kbits/s </br>",
                               slow_hh_ffm , "% of the route has less than 32 kbits/s")
  return (popupcontent_hhffm)
}
info_munchcol <- function (global){
  popupcontent_munchcol <-  paste("<b><a>ICE Train route Munich Cologne</a></b></br>",
                                  "Average Downloadrate: ", avg_munch_koe," kbits/s </br>",
                                  "Standard deviation: ", sd_munch_koe," kbits/s </br>",
                                  slow_munch_koe , "% of the route has less than 32 kbits/s")
  return (popupcontent_munchcol)
}
info_munchffm <- function (global){
  popupcontent_munchffm <-  paste("<b><a>ICE Train route Munich Frankfurt</a></b></br>",
                                  "Average Downloadrate: ", avg_munch_ffm," kbits/s </br>",
                                  "Standard deviation: ", sd_munch_ffm," kbits/s </br>",
                                  slow_munch_ffm , "% of the route has less than 32 kbits/s")
  return (popupcontent_munchffm)
}
info_colffm <- function (global){
  popupcontent_colffm <-  paste("<b><a>ICE Train route Frankfurt Cologne</a></b></br>",
                                "Average Downloadrate: ", avg_koe_ffm," kbits/s </br>",
                                "Standard deviation: ", sd_koe_ffm," kbits/s </br>",
                                slow_koe_ffm , "% of the route has less than 32 kbits/s")
  return (popupcontent_colffm)
}
















