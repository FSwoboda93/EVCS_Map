#Module ui function
geoDataOutput5 <- function(id) {
  ns <- NS(id)
  #write your code here
  
  tagList(
    # radioButtons(ns("radio"), 
    #              NULL,
    #              choices = list("Daily"="d","Weekly"="w"),
    #              selected = "d", 
    #              inline = TRUE),
    
    htmlOutput(ns("text5")),
    plotOutput(ns("diagram5"))
    
    
  )
  #end your code here

}