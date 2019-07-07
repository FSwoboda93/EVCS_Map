#Module ui function
geoDataOutput3_1 <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("PieChart"))
    )}
  
geoDataOutput3_2 <- function(id) {
  ns <- NS(id)
  tagList(
    #plotOutput 
    plotlyOutput(ns("avg_distance_chart"))
    )}
  
    #end your code here

