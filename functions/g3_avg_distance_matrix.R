# group3 - OpenChargeMap
# here we calculate all the distance between each POI
# this might take up to 2 minutes processing, but it must be done only once, everytime a new data set is downloaded from server

library(gdistance)
library(Imap)


   # --- read CSV file
  
  #--- load new csv file
    #ocm_file <- "C:/Users/Swob1/OneDrive/Desktop/2018-ws-mobility-dashboard/Dashboard/datafiles/database_group3.csv"
    #download.file("https://api.openchargemap.io/v2/poi/?output=csv&countrycode=DE&maxresults=15000", ocm_file)
    #OCMData <- read.csv(ocm_file)
    
    #--- Option 2
    #OCMData <- read.csv(file.choose(), header=TRUE)
    
    #---option 3
    OCMData <- read.csv(file = "../2018-ws-mobility-dashboard/datafiles/OpenChargeMap/database_group3.csv" , header = TRUE) 
    

  # ---explore and fix data ---
  #summary(OCMData)
  unique(OCMData$UsageType)
  colnames(OCMData)[1] <- "ID"
  OCMData$Postcode <- as.numeric(as.character(OCMData$Postcode))


 # ---variable declaration ---
  vector_cities <- c("Berlin","Hamburg","Frankfurt","Cologne","Munich")
  
  #--- One-time run Functions (execute first) ---
  # -------
  # Calculating a Distance Matrix for Geographic Points Using R
  # Retrieved from https://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/
  # -------
  
  # calculate a distance matrix for geographic points (expressed as decimal latitudes and longitudes)
  # in straight-line distance-accounting for curvature of the earth.
  
  
  ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
    # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
    # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
    
    if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
    if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
    else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
    else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
    m[tri] <- t(m)[tri]
    return(m)
  }
  
  
  GeoDistanceInMetresMatrix <- function(df.geopoints){
    # Returns a matrix (M) of distances between geographic points.
    # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
    # (df.geopoints$lat[j], df.geopoints$lon[j]).
    # The row and column names are given by df.geopoints$name.
    
    GeoDistanceInMetres <- function(g1, g2){
      # Returns a vector of distances. (But if g1$index > g2$index, or distance between g1 and g2 > 60km, returns zero.)
      # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
      # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
      # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
      # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
      DistM <- function(g1, g2){
        require("Imap")
        distgg <- gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")
        #return(ifelse(g1$index > g2$index, 0, distgg))
        return(ifelse(g1$index > g2$index, 0, ifelse(distgg<=60000,distgg,0)))
      }
      return(mapply(DistM, g1, g2))
    }
    
    n.geopoints <- nrow(df.geopoints)
    
    # The index column is used to ensure we only do calculations for the upper triangle of points
    df.geopoints$index <- 1:n.geopoints
    
    # Create a list of lists
    list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
    
    # Get a matrix of distances (in metres)
    mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
    
    # Set the row and column names
    rownames(mat.distances) <- df.geopoints$name
    colnames(mat.distances) <- df.geopoints$name
    
    return(mat.distances)
  }
  
  
  # ------
  # Function to calculate avg distance between all points
  # ------
  
  avg_distance <- function (selected_city){
    #selected_city <- "K?ln"
    
    ifelse(selected_city %in% "Munich",
           df.city <- OCMData[grepl('Muenchen|Munich|München|Munchen|MÃ¼nchen',OCMData$StateOrProvince) | grepl('Muenchen|Munich|Munchen|Munchen|MÃ¼nchen',OCMData$Town),],
           ifelse(selected_city %in% "Cologne",
                  df.city <- OCMData[grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$StateOrProvince) | grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$Town),],
                  df.city <- OCMData[grepl(selected_city,OCMData$StateOrProvince) | grepl(selected_city,OCMData$Town),]))
    
    df.city <- df.city[which
                       (df.city$Postcode < (postcode(selected_city)+9999) & 
                        df.city$Postcode >= postcode(selected_city)),]
    
    
    # Rename columns
    colnames(df.city)[colnames(df.city)=="ID"] <- "index"
    colnames(df.city)[colnames(df.city)=="Latitude"] <- "lat"
    colnames(df.city)[colnames(df.city)=="Longitude"] <- "lon"
    
    y <- round(GeoDistanceInMetresMatrix(df.city) / 1000, digits=2)
    
    return(mean(y))  
    
  }
  
  avg_public_distance <- function (selected_city){
    #selected_city <- "K?ln"
    
    ifelse(selected_city %in% "Munich",
           df.city <- OCMData[grepl('Muenchen|Munich|München|Munchen|MÃ¼nchen',OCMData$StateOrProvince) | grepl('Muenchen|Munich|Munchen|Munchen|MÃ¼nchen',OCMData$Town),],
           ifelse(selected_city %in% "Cologne",
                  df.city <- OCMData[grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$StateOrProvince) | grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$Town),],
                  df.city <- OCMData[grepl(selected_city,OCMData$StateOrProvince) | grepl(selected_city,OCMData$Town),]))
    
    df.city <- df.city[which
                       (df.city$Postcode < (postcode(selected_city)+9999) & 
                        df.city$Postcode >= postcode(selected_city) & 
                        df.city$UsageType == "Public" |
                        df.city$UsageType == "Public - Membership Required" |
                        df.city$UsageType == "Public - Pay At Location" |
                        df.city$UsageType == "Public - Notice Required"),]
    
    
    # Rename columns
    colnames(df.city)[colnames(df.city)=="ID"] <- "index"
    colnames(df.city)[colnames(df.city)=="Latitude"] <- "lat"
    colnames(df.city)[colnames(df.city)=="Longitude"] <- "lon"
    
    y <- round(GeoDistanceInMetresMatrix(df.city) / 1000, digits=2)
    
    return(mean(y))  
    
  }
  
  avg_private_distance <- function (selected_city){
    #selected_city <- "K?ln"
    
    ifelse(selected_city %in% "Munich",
           df.city <- OCMData[grepl('Muenchen|Munich|München|Munchen|MÃ¼nchen',OCMData$StateOrProvince) | grepl('Muenchen|Munich|Munchen|Munchen|MÃ¼nchen',OCMData$Town),],
           ifelse(selected_city %in% "Cologne",
                  df.city <- OCMData[grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$StateOrProvince) | grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$Town),],
                  df.city <- OCMData[grepl(selected_city,OCMData$StateOrProvince) | grepl(selected_city,OCMData$Town),]))
    
    df.city <- df.city[which
                       (df.city$Postcode < (postcode(selected_city)+9999) & 
                           df.city$Postcode >= postcode(selected_city) & 
                           df.city$UsageType == "Private - For Staff, Visitors or Customers" |
                           df.city$UsageType == "Private - Restricted Access" |
                           df.city$UsageType == "Privately Owned - Notice Required"),]
    
    
    # Rename columns
    colnames(df.city)[colnames(df.city)=="ID"] <- "index"
    colnames(df.city)[colnames(df.city)=="Latitude"] <- "lat"
    colnames(df.city)[colnames(df.city)=="Longitude"] <- "lon"
    
    y <- round(GeoDistanceInMetresMatrix(df.city) / 1000, digits=2)
    
    return(mean(y))  
    
  }
  
  avg_unknown_distance <- function (selected_city){
    #selected_city <- "K?ln"
    
    ifelse(selected_city %in% "Munich",
           df.city <- OCMData[grepl('Muenchen|Munich|München|Munchen|MÃ¼nchen',OCMData$StateOrProvince) | grepl('Muenchen|Munich|Munchen|Munchen|MÃ¼nchen',OCMData$Town),],
           ifelse(selected_city %in% "Cologne",
                  df.city <- OCMData[grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$StateOrProvince) | grepl('Köln|Cologne|Keulen|Koeln|KÃ¶ln',OCMData$Town),],
                  df.city <- OCMData[grepl(selected_city,OCMData$StateOrProvince) | grepl(selected_city,OCMData$Town),]))
    
    df.city <- df.city[which
                       (df.city$Postcode < (postcode(selected_city)+9999) & 
                           df.city$Postcode >= postcode(selected_city) & 
                           df.city$UsageType == "(Unknown)" |
                           df.city$UsageType == "" |
                           is.na(df.city$UsageType)),]
    
    
    # Rename columns
    colnames(df.city)[colnames(df.city)=="ID"] <- "index"
    colnames(df.city)[colnames(df.city)=="Latitude"] <- "lat"
    colnames(df.city)[colnames(df.city)=="Longitude"] <- "lon"
    
    y <- round(GeoDistanceInMetresMatrix(df.city) / 1000, digits=2)
    
    return(mean(y))  
    
  }
  
  postcode <- function(selected_city){
    
    pc <- ifelse(selected_city %in% vector_cities[1],10000, #berlin
                 ifelse(selected_city %in% vector_cities[2],20000, #hamburg
                        ifelse(selected_city %in% vector_cities[3],60000, #frankfurt
                               ifelse(selected_city %in% vector_cities[4],50000, #cologne
                                      ifelse(selected_city %in% vector_cities[5],80000) #munich
                               )
                        )
                 )
    )
    return(pc)
    
  }
  
  # first, load  one-time run functions below
  # then calculate (here below) average distance only once (wait around 40 seconds)
  
  calcuulate_all_avg_distance <- function (){
    
  vector_average_distance <<- if ("vector_average_distance" %in% ls(envir = .GlobalEnv)) 
    vector_average_distance
  else
    c(avg_distance(vector_cities[1]),
      avg_distance(vector_cities[2]),
      avg_distance(vector_cities[3]),
      avg_distance(vector_cities[4]),
      avg_distance(vector_cities[5]))
  
  vector_public_distance <<- if ("vector_public_distance" %in% ls(envir = .GlobalEnv)) 
    vector_public_distance
  else
    c(avg_public_distance(vector_cities[1]),
      avg_public_distance(vector_cities[2]),
      avg_public_distance(vector_cities[3]),
      avg_public_distance(vector_cities[4]),
      avg_public_distance(vector_cities[5]))
  
  vector_private_distance <<- if ("vector_private_distance" %in% ls(envir = .GlobalEnv)) 
    vector_private_distance
  else
    c(avg_private_distance(vector_cities[1]),
      avg_private_distance(vector_cities[2]),
      avg_private_distance(vector_cities[3]),
      avg_private_distance(vector_cities[4]),
      avg_private_distance(vector_cities[5]))
  
  vector_unknown_distance <<- if ("vector_unknown_distance" %in% ls(envir = .GlobalEnv)) 
    vector_unknown_distance
  else
    c(avg_unknown_distance(vector_cities[1]),
      avg_unknown_distance(vector_cities[2]),
      avg_unknown_distance(vector_cities[3]),
      avg_unknown_distance(vector_cities[4]),
      avg_unknown_distance(vector_cities[5]))
  
  
  
  df.all_avg_distance <- data.frame(vector_cities, vector_average_distance, vector_public_distance, vector_private_distance, vector_unknown_distance)
  return(df.all_avg_distance)
  
  }

  
  df.all_avg_distance <- calcuulate_all_avg_distance()
  write.csv(df.all_avg_distance, file="../2018-ws-mobility-dashboard/datafiles/OpenChargeMap/database_group3_avg_distances.csv", row.name=TRUE)
  