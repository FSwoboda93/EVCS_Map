#Module ui function
geoDataOutput1 <- function(id) {
  ns <- NS(id)
  #write your code here  
  tagList(
  plotlyOutput(ns("diagram1"))
  )
 
}

geoDataOutput1_2 <- function(id) {
  ns <- NS(id)
  #write your code here  
  tagList(
    plotlyOutput(ns("diagram2"))
  )
  
}