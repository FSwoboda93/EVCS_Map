library(fmsb)

geoData2 <- function(input, output, session, global, stringsAsFactors = FALSE){  
  observe({
    filtercity <- global$location
  
  
  
  #server <- shinyServer(function(input, output) {
    cities <- read.csv2(file = "datafiles/OSM/OSM_data.csv" , header = TRUE, sep = ";", dec = ".", fill = TRUE) 
    
    
    hamburg <- subset(cities,cities$city=="Hamburg")
    
    berlin <- subset(cities,cities$city=="Berlin")
    frankfurt <- subset(cities,cities$city=="Frankfurt")
    koeln <- subset(cities,cities$city=="Cologne")
    hamburg <- subset(cities,cities$city=="Hamburg")
    muenchen <- subset(cities,cities$city=="Munich")
    
    ferry_filter<-subset(berlin,berlin$amenity=="ferry_terminal")
    ferry_b <- length(ferry_filter$amenity)
    ferry_filter<-subset(frankfurt,frankfurt$amenity=="ferry_terminal")
    ferry_f <- length(ferry_filter$amenity)
    ferry_filter<-subset(koeln,koeln$amenity=="ferry_terminal")
    ferry_k <- length(ferry_filter$amenity)
    ferry_filter<-subset(hamburg,hamburg$amenity=="ferry_terminal")
    ferry_h <- length(ferry_filter$amenity)
    ferry_filter<-subset(muenchen,muenchen$amenity=="ferry_terminal")
    ferry_m <- length(ferry_filter$amenity)
    
    ferries <- matrix(c(ferry_b,ferry_f,ferry_k,ferry_h,ferry_m))
    ferry_min <- min(ferries)
    ferry_max <- max(ferries)
    
    
    taxi_filter<-subset(berlin,berlin$amenity=="taxi")
    taxi_b <- length(taxi_filter$amenity)
    taxi_filter<-subset(frankfurt,frankfurt$amenity=="taxi")
    taxi_f <- length(taxi_filter$amenity)
    taxi_filter<-subset(koeln,koeln$amenity=="taxi")
    taxi_k <- length(taxi_filter$amenity)
    taxi_filter<-subset(hamburg,hamburg$amenity=="taxi")
    taxi_h <- length(taxi_filter$amenity)
    taxi_filter<-subset(muenchen,muenchen$amenity=="taxi")
    taxi_m <- length(taxi_filter$amenity)
    
    taxi <- matrix(c(taxi_b,taxi_f,taxi_k,taxi_h,taxi_m))
    taxi_min <- min(taxi)
    taxi_max <- max(taxi)
    
    
    car_rental_filter<-subset(berlin,berlin$amenity=="car_rental")
    car_rental_b <- length(car_rental_filter$amenity)
    car_rental_filter<-subset(frankfurt,frankfurt$amenity=="car_rental")
    car_rental_f <- length(car_rental_filter$amenity)
    car_rental_filter<-subset(koeln,koeln$amenity=="car_rental")
    car_rental_k <- length(car_rental_filter$amenity)
    car_rental_filter<-subset(hamburg,hamburg$amenity=="car_rental")
    car_rental_h <- length(car_rental_filter$amenity)
    car_rental_filter<-subset(muenchen,muenchen$amenity=="car_rental")
    car_rental_m <- length(car_rental_filter$amenity)
    
    car_rental <- matrix(c(car_rental_b,car_rental_f,car_rental_k,car_rental_h,car_rental_m))
    car_rental_min <- min(car_rental)
    car_rental_max <- max(car_rental)
    
    car_sharing_filter<-subset(berlin,berlin$amenity=="car_sharing")
    car_sharing_b <- length(car_sharing_filter$amenity)
    car_sharing_filter<-subset(frankfurt,frankfurt$amenity=="car_sharing")
    car_sharing_f <- length(car_sharing_filter$amenity)
    car_sharing_filter<-subset(koeln,koeln$amenity=="car_sharing")
    car_sharing_k <- length(car_sharing_filter$amenity)
    car_sharing_filter<-subset(hamburg,hamburg$amenity=="car_sharing")
    car_sharing_h <- length(car_sharing_filter$amenity)
    car_sharing_filter<-subset(muenchen,muenchen$amenity=="car_sharing")
    car_sharing_m <- length(car_sharing_filter$amenity)
    
    car_sharing <- matrix(c(car_sharing_b,car_sharing_f,car_sharing_k,car_sharing_h,car_sharing_m))
    car_sharing_min <- min(car_sharing)
    car_sharing_max <- max(car_sharing)
    
    bicycle_rental_filter<-subset(berlin,berlin$amenity=="bicycle_rental")
    bicycle_rental_b <- length(bicycle_rental_filter$amenity)
    bicycle_rental_filter<-subset(frankfurt,frankfurt$amenity=="bicycle_rental")
    bicycle_rental_f <- length(bicycle_rental_filter$amenity)
    bicycle_rental_filter<-subset(koeln,koeln$amenity=="bicycle_rental")
    bicycle_rental_k <- length(bicycle_rental_filter$amenity)
    bicycle_rental_filter<-subset(hamburg,hamburg$amenity=="bicycle_rental")
    bicycle_rental_h <- length(bicycle_rental_filter$amenity)
    bicycle_rental_filter<-subset(muenchen,muenchen$amenity=="bicycle_rental")
    bicycle_rental_m <- length(bicycle_rental_filter$amenity)
    
    bicycle_rental <- matrix(c(bicycle_rental_b,bicycle_rental_f,bicycle_rental_k,bicycle_rental_h,bicycle_rental_m))
    bicycle_rental_min <- min(bicycle_rental)
    bicycle_rental_max <- max(bicycle_rental)
    
    tram_filter<-subset(berlin,berlin$amenity=="tram_stop")
    tram_b <- length(tram_filter$amenity)
    tram_filter<-subset(frankfurt,frankfurt$amenity=="tram_stop")
    tram_f <- length(tram_filter$amenity)
    tram_filter<-subset(koeln,koeln$amenity=="tram_stop")
    tram_k <- length(tram_filter$amenity)
    tram_filter<-subset(hamburg,hamburg$amenity=="tram_stop")
    tram_h <- length(tram_filter$amenity)
    tram_filter<-subset(muenchen,muenchen$amenity=="tram_stop")
    tram_m <- length(tram_filter$amenity)
    
    tram <- matrix(c(tram_b,tram_f,tram_k,tram_h,tram_m))
    tram_min <- min(tram)
    tram_max <- max(tram)
    
    bus_filter<-subset(berlin,berlin$amenity=="bus_stop")
    bus_b <- length(bus_filter$amenity)
    bus_filter<-subset(frankfurt,frankfurt$amenity=="bus_stop")
    bus_f <- length(bus_filter$amenity)
    bus_filter<-subset(koeln,koeln$amenity=="bus_stop")
    bus_k <- length(bus_filter$amenity)
    bus_filter<-subset(hamburg,hamburg$amenity=="bus_stop")
    bus_h <- length(bus_filter$amenity)
    bus_filter<-subset(muenchen,muenchen$amenity=="bus_stop")
    bus_m <- length(bus_filter$amenity)
    
    bus <- matrix(c(bus_b,bus_f,bus_k,bus_h,bus_m))
    bus_min <- min(bus)
    bus_max <- max(bus)
    
    subway_filter<-subset(berlin,berlin$amenity=="station")
    subway_b <- length(subway_filter$amenity)
    subway_filter<-subset(frankfurt,frankfurt$amenity=="station")
    subway_f <- length(subway_filter$amenity)
    subway_filter<-subset(koeln,koeln$amenity=="station")
    subway_k <- length(subway_filter$amenity)
    subway_filter<-subset(hamburg,hamburg$amenity=="station")
    subway_h <- length(subway_filter$amenity)
    subway_filter<-subset(muenchen,muenchen$amenity=="station")
    subway_m <- length(subway_filter$amenity)
    
    subway <- matrix(c(subway_b,subway_f,subway_k,subway_h,subway_m))
    subway_min <- min(subway)
    subway_max <- max(subway)
    
    train_filter<-subset(berlin,berlin$amenity=="halt")
    train_b <- length(train_filter$amenity)
    train_filter<-subset(frankfurt,frankfurt$amenity=="halt")
    train_f <- length(train_filter$amenity)
    train_filter<-subset(koeln,koeln$amenity=="halt")
    train_k <- length(train_filter$amenity)
    train_filter<-subset(hamburg,hamburg$amenity=="halt")
    train_h <- length(train_filter$amenity)
    train_filter<-subset(muenchen,muenchen$amenity=="halt")
    train_m <- length(train_filter$amenity)
    
    train <- matrix(c(train_b,train_f,train_k,train_h,train_m))
    train_min <- min(train)
    train_max <- max(train)
    
    ham_selected_ferry<- (100*ferry_h/ferry_max)
    ham_selected_taxi <- (100*taxi_h/taxi_max)
    ham_selected_car <- (100*car_rental_h/car_rental_max)
    ham_selected_carsharing <- (100*car_sharing_h/car_sharing_max)
    ham_selected_bicycle <- (100*bicycle_rental_h/bicycle_rental_max)
    ham_selected_tram <- (100*tram_h/tram_max)
    ham_selected_bus <- (100*bus_h/bus_max)
    ham_selected_subway <- (100*subway_h/subway_max)
    ham_selected_train <- (100*train_h/train_max)
    
    fra_selected_ferry<- (100*ferry_f/ferry_max)
    fra_selected_taxi <- (100*taxi_f/taxi_max)
    fra_selected_car <- (100*car_rental_f/car_rental_max)
    fra_selected_carsharing <- (100*car_sharing_f/car_sharing_max)
    fra_selected_bicycle <- (100*bicycle_rental_f/bicycle_rental_max)
    fra_selected_tram <- (100*tram_f/tram_max)
    fra_selected_bus <- (100*bus_f/bus_max)
    fra_selected_subway <- (100*subway_f/subway_max)
    fra_selected_train <- (100*train_f/train_max)
    
    ber_selected_ferry = (100*ferry_b/ferry_max)
    ber_selected_taxi <- (100*taxi_b/taxi_max)
    ber_selected_car <- (100*car_rental_b/car_rental_max)
    ber_selected_carsharing <- (100*car_sharing_b/car_sharing_max)
    ber_selected_bicycle <- (100*bicycle_rental_b/bicycle_rental_max)
    ber_selected_tram <- (100*tram_b/tram_max)
    ber_selected_bus <- (100*bus_b/bus_max)
    ber_selected_subway <- (100*subway_b/subway_max)
    ber_selected_train <- (100*train_b/train_max)
    
    cgn_selected_ferry<- (100*ferry_k/ferry_max)
    cgn_selected_taxi <- (100*taxi_k/taxi_max)
    cgn_selected_car <- (100*car_rental_k/car_rental_max)
    cgn_selected_carsharing <- (100*car_sharing_k/car_sharing_max)
    cgn_selected_bicycle <- (100*bicycle_rental_k/bicycle_rental_max)
    cgn_selected_tram <- (100*tram_k/tram_max)
    cgn_selected_bus <- (100*bus_k/bus_max)
    cgn_selected_subway <- (100*subway_k/subway_max)
    cgn_selected_train <- (100*train_k/train_max)
    
    muc_selected_ferry<- (100*ferry_m/ferry_max)
    muc_selected_taxi <- (100*taxi_m/taxi_max)
    muc_selected_car <- (100*car_rental_m/car_rental_max)
    muc_selected_carsharing <- (100*car_sharing_m/car_sharing_max)
    muc_selected_bicycle <- (100*bicycle_rental_m/bicycle_rental_max)
    muc_selected_tram <- (100*tram_m/tram_max)
    muc_selected_bus <- (100*bus_m/bus_max)
    muc_selected_subway <- (100*subway_m/subway_max)
    muc_selected_train <- (100*train_m/train_max)
    
    hamburgplot <- 
    
    #Hamburg
    if(filtercity == "Hamburg"){
     output$diagram2 <- renderPlot({
      
      # Create data
      data = as.data.frame(matrix(c(ham_selected_ferry,ham_selected_taxi,ham_selected_car,
                                    ham_selected_carsharing,ham_selected_bicycle,ham_selected_tram,
                                    ham_selected_bus,ham_selected_subway,ham_selected_train) , ncol = 9))
    colnames(data) = c("ferry landing places
[0:46]", 
                       "taxi rentals
[54,376]", 
                       "car rentals
[27,84]", 
                       "car sharing stations
[21,107]", 
                       "bicycle rentals
[2,210]",
                       "tram stops
[0,806]", 
                       "bus stops
[1309,6045]", 
                       "subway stops
[17,273]", 
                       "train stops
[7,77]")
    #rownames(data) = paste("mister" , letters[1:3] , sep = "-")
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    data = rbind(rep(100, 9) , rep(0, 9) , data)
    
    colors_border = c(rgb(1.0,0.549,0.0,0.7))
    colors_in = c(rgb(1.0,0.549,0.0,0.3))
    radarchart(
      data  ,
      axistype = 1 ,
      #custom polygon
      pcol = colors_border ,
      pfcol = colors_in ,
      plwd = 4 ,
      plty = 1,
      #custom the grid
      cglcol = rgb(0.055,0.647,1.0,0.6),
      cglty = 1,
      axislabcol = "grey",
      title = "Mobility overview of available cities
(0% smallest city and 100% biggest city)",
      caxislabels = seq(0,100,25),
      cglwd = 0.8,
      #custom labels
      vlcex = 1
      
    )
    
})
    }
    if(filtercity == "Berlin"){
      #Berlin
      output$diagram2 <- renderPlot({
        
        # Create data
        data = as.data.frame(matrix(c(ber_selected_ferry,ber_selected_taxi,ber_selected_car,
                                      ber_selected_carsharing,ber_selected_bicycle,ber_selected_tram,
                                      ber_selected_bus,ber_selected_subway,ber_selected_train) , ncol = 9))
        colnames(data) = c("ferry landing places
[0:46]", 
                           "taxi rentals
[54,376]", 
                           "car rentals
[27,84]", 
                           "car sharing stations
[21,107]", 
                           "bicycle rentals
[2,210]",
                           "tram stops
[0,806]", 
                           "bus stops
[1309,6045]", 
                           "subway stops
[17,273]", 
                           "train stops
[7,77]")
        #rownames(data) = paste("mister" , letters[1:3] , sep = "-")
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
        data = rbind(rep(100, 9) , rep(0, 9) , data)
        
        colors_border = c(rgb(1.0,0.549,0.0,0.7))
        colors_in = c(rgb(1.0,0.549,0.0,0.3))
        radarchart(
          data  ,
          axistype = 1 ,
          #custom polygon
          pcol = colors_border ,
          pfcol = colors_in ,
          plwd = 4 ,
          plty = 1,
          #custom the grid
          cglcol = rgb(0.055,0.647,1.0,0.6),
          cglty = 1,
          axislabcol = "grey",
          title = "Mobility overview of available cities
(0% smallest city and 100% biggest city)",
          caxislabels = seq(0,100,25),
          cglwd = 0.8,
          #custom labels
          vlcex = 1
          
        )
        
      })
    }
    if(filtercity == "Frankfurt"){
      #Frankfurt
      output$diagram2 <- renderPlot({
        
        # Create data
        data = as.data.frame(matrix(c(fra_selected_ferry,fra_selected_taxi,fra_selected_car,
                                      fra_selected_carsharing,fra_selected_bicycle,fra_selected_tram,
                                      fra_selected_bus,fra_selected_subway,fra_selected_train) , ncol = 9))
        colnames(data) = c("ferry landing places
[0:46]", 
                           "taxi rentals
[54,376]", 
                           "car rentals
[27,84]", 
                           "car sharing stations
[21,107]", 
                           "bicycle rentals
[2,210]",
                           "tram stops
[0,806]", 
                           "bus stops
[1309,6045]", 
                           "subway stops
[17,273]", 
                           "train stops
[7,77]")
        #rownames(data) = paste("mister" , letters[1:3] , sep = "-")
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
        data = rbind(rep(100, 9) , rep(0, 9) , data)
        
        colors_border = c(rgb(1.0,0.549,0.0,0.7))
        colors_in = c(rgb(1.0,0.549,0.0,0.3))
        radarchart(
          data  ,
          axistype = 1 ,
          #custom polygon
          pcol = colors_border ,
          pfcol = colors_in ,
          plwd = 4 ,
          plty = 1,
          #custom the grid
          cglcol = rgb(0.055,0.647,1.0,0.6),
          cglty = 1,
          axislabcol = "grey",
          title = "Mobility overview of available cities
(0% smallest city and 100% biggest city)",
          caxislabels = seq(0,100,25),
          cglwd = 0.8,
          #custom labels
          vlcex = 1
          
        )
        
      })
    }
    if(filtercity == "Cologne"){
      #Colonge
      output$diagram2 <- renderPlot({
        
        # Create data
        data = as.data.frame(matrix(c(cgn_selected_ferry,cgn_selected_taxi,cgn_selected_car,
                                      cgn_selected_carsharing,cgn_selected_bicycle,cgn_selected_tram,
                                      cgn_selected_bus,cgn_selected_subway,cgn_selected_train) , ncol = 9))
        colnames(data) = c("ferry landing places
[0:46]", 
                           "taxi rentals
[54,376]", 
                           "car rentals
[27,84]", 
                           "car sharing stations
[21,107]", 
                           "bicycle rentals
[2,210]",
                           "tram stops
[0,806]", 
                           "bus stops
[1309,6045]", 
                           "subway stops
[17,273]", 
                           "train stops
[7,77]")
        #rownames(data) = paste("mister" , letters[1:3] , sep = "-")
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
        data = rbind(rep(100, 9) , rep(0, 9) , data)
        
        colors_border = c(rgb(1.0,0.549,0.0,0.7))
        colors_in = c(rgb(1.0,0.549,0.0,0.3))
        radarchart(
          data  ,
          axistype = 1 ,
          #custom polygon
          pcol = colors_border ,
          pfcol = colors_in ,
          plwd = 4 ,
          plty = 1,
          #custom the grid
          cglcol = rgb(0.055,0.647,1.0,0.6),
          cglty = 1,
          axislabcol = "grey",
          title = "Mobility overview of available cities
(0% smallest city and 100% biggest city)",
          caxislabels = seq(0,100,25),
          cglwd = 0.8,
          #custom labels
          vlcex = 1
          
        )
        
      })
    }
    if(filtercity == "Munich"){
      #Munich
      output$diagram2 <- renderPlot({
        
        # Create data
        data = as.data.frame(matrix(c(muc_selected_ferry,muc_selected_taxi,muc_selected_car,
                                      muc_selected_carsharing,muc_selected_bicycle,muc_selected_tram,
                                      muc_selected_bus,muc_selected_subway,muc_selected_train) , ncol = 9))
        colnames(data) = c("ferry landing places
[0:46]", 
                           "taxi rentals
[54,376]", 
                           "car rentals
[27,84]", 
                           "car sharing stations
[21,107]", 
                           "bicycle rentals
[2,210]",
                           "tram stops
[0,806]", 
                           "bus stops
[1309,6045]", 
                           "subway stops
[17,273]", 
                           "train stops
[7,77]")
        #rownames(data) = paste("mister" , letters[1:3] , sep = "-")
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
        data = rbind(rep(100, 9) , rep(0, 9) , data)
        
        colors_border = c(rgb(1.0,0.549,0.0,0.7))
        colors_in = c(rgb(1.0,0.549,0.0,0.3))
        radarchart(
          data  ,
          axistype = 1 ,
          #custom polygon
          pcol = colors_border ,
          pfcol = colors_in ,
          plwd = 4 ,
          plty = 1,
          #custom the grid
          cglcol = rgb(0.055,0.647,1.0,0.6),
          cglty = 1,
          axislabcol = "grey",
          title = "Mobility overview of available cities
(0% smallest city and 100% biggest city)",
          caxislabels = seq(0,100,25),
          cglwd = 0.8,
          #custom labels
          vlcex = 1
          
        )
        
      })
    }
 
  })  
}



