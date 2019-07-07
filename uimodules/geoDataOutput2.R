#Module ui function
geoDataOutput2 <- function(id) {
  ns <- NS(id)
  #write your code here
  plotOutput(ns("diagram2"))
  #end your code here
 
}