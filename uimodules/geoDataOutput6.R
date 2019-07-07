#Module ui function
geoDataOutput6 <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("diagram6"), hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))
  )
  
}
geoDataOutput6_infobox <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("hover_info")))
}