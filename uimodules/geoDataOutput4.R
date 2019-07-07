#Module ui function
geoDataOutput41 <- function(id) {
  ns <- NS(id)

  
  #write your code here
  fluidRow(
    fluidRow(column(11, offset = 1, h3("Tweet frequency for a selected city per query topic"))),
    column(11, offset = 1,tagList(plotOutput(ns("diagram41"))))) 
}

geoDataOutput42 <- function(id){
  ns <- NS(id)
  
  fluidRow(
    fluidRow(column(11, offset = 1, h4("Word frequency (as node size) and pairwise word occurence (as line width)"))),
    column(11, offset = 1, tagList(plotOutput(ns("diagram42")))))
  
}

geoDataOutput43 <- function(id){
  ns <- NS(id)
  valueBoxOutput(ns("TrainBox"))
}

geoDataOutput44 <- function(id){
  ns <- NS(id)
  valueBoxOutput(ns("TrafficBox"))
}
