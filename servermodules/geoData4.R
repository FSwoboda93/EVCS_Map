library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tidytext)
library(tm)
library(widyr)
library(igraph)
library(ggraph)
library(leaflet.minicharts)
library(plotly)


tweets <- read.csv(file = "datafiles/Twitter/tweets.csv", encoding = "UTF-8")
tweets$created_at <- ymd_hms(tweets$created_at)
tweets$text <- as.character(tweets$text)
tweets$status_id <- as.character(tweets$status_id)

car_tweets <- filter(tweets, domain == "traffic")
train_tweets <- filter(tweets, domain == "train")

# Module server function
geoData4 <- function(input, output, session, global) {
  
  
observe({

  
#--------Input
  
  
  #Handling of dashboard user input
  displayed_city <- tolower(as.character(global$location))
  graph_type <- as.character(global$graphG4)
  displayed_period <- as.character(global$period)
  displayed_date_start <- ymd(global$dateG4)
  displayed_date_end <- ymd(global$dateG4) + weeks(1)
   
  #Calculaction of the displayed date range according to both date pickers
  if(displayed_period == "three-days"){displayed_date_end <- as.Date(displayed_date_start) + days(3)} 
  else if(displayed_period == "weekly"){displayed_date_end <- as.Date(displayed_date_start) + weeks(1)} 
  else if (displayed_period == "monthly") {displayed_date_end <- as.Date(displayed_date_start) %m+% months(1)} 
  else {displayed_date_end <- as.Date(displayed_date_start) + years(1)}
  
  #Transform input city names, e.g. Munich -> München
  if(global$location == "Cologne"){displayed_city <- "koeln"} 
  else if(global$location == "Munich"){displayed_city <- "muenchen"} 
 
  
  
#--------Graph1: Start
  
  
  #First extracting tweet text & date, then filtering date and city according to the dashboard input
  train_texts <- dplyr::select(train_tweets,text, created_at) %>%
    filter(created_at >= displayed_date_start, created_at < displayed_date_end, train_tweets$city == displayed_city)
  
  #Transforming the date format and counting the Tweet freq by time unit
  train_texts$created_at <- format(train_texts$created_at,format='%Y-%m-%d %H')
  train_texts <- count(train_texts, created_at)
  train_texts$created_at <- ymd_h(train_texts$created_at)
  
  #Same goes for tweets about traffic
  car_texts <- dplyr::select(car_tweets,text, created_at) %>%
    filter(created_at >= displayed_date_start, created_at < displayed_date_end, car_tweets$city == displayed_city)
  
  car_texts$created_at <- format(car_texts$created_at,format='%Y-%m-%d %H')
  car_texts <- count(car_texts, created_at)
  car_texts$created_at <- ymd_h(car_texts$created_at)
  
  #Data is merged into one data frame, na handling
  line_data <- merge(car_texts, train_texts, all = TRUE, by = "created_at")
  line_data[is.na(line_data)] <- 0
  line_data <- rename(line_data, Traffic = n.x, Train = n.y)
  
  #Plot line Graph
  line_plot <- ggplot()+
    geom_line(data=line_data,aes(y=Traffic,x= created_at,colour= "Traffic"),size=1 )+
    geom_line(data=line_data,aes(y=Train,x= created_at,colour= "Train"),size=1) +
    labs(title="", y="No. of Tweets", x = "Time") + 
    theme_minimal() +
    scale_color_manual(name= "Twitter Query", values=c("#FF8C00", "#0EA5FF")) +
    theme(plot.title = element_text(hjust=-1, color="black"),
          axis.title.y = element_text(color="black", size=14, face = "bold"),
          axis.title.x = element_text(color="black", size=14, face = "bold"),
          axis.text = element_text(color ="black", size = 12),
          legend.title = element_text(size = 11, face= "bold", colour = "black"),
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(color= "black", size = 12),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))
  
#-------Graph1: Ende
  

#-------Graph2: Start
  
  set.seed(1234)
  
  #Filter the data according to dashboard input (date, city), select needed columns and extract single words
  car_words <- filter(car_tweets, displayed_city == car_tweets$city,created_at > displayed_date_start, created_at < displayed_date_end) %>%
    dplyr::select(text, status_id) %>% 
    unnest_tokens(word, text) 
  
  #Word cleaning: remove the words you dont want to visualize
  stopwords <- stopwords("german")
  stopwords <- data.frame(stopwords)
  colnames(stopwords)[1] <- "word"
  words_to_remove <- c("immer", "sagt", "gibt", "schön","fest", "danke", "heute", "sorgt", "mehr", "weiterhin", "konnte","beim", "uhr", "ca", "nie", "t.co", "abfahrt", "aktuelle", "betroffener", "fahrt", "vorheriger", "minuten", "wegen", "planmäßig", "min", "grund", "notwendige", "min", "https", "hamburg")
  
  stopwords$word <- as.character(stopwords$word)
  car_clean <- car_words %>% anti_join(stopwords, by ="word") %>% filter(!word %in% words_to_remove)
  car_clean$word <- gsub('[0-9]{2,}', '', car_clean$word)
  car_clean <- filter(car_clean, !word == "")
  
  #Edges: stores word pairs and the number of occurences of the pair, node_size stores the frequency of a word
  edges <-  car_clean %>% 
    pairwise_count(word, status_id, sort = TRUE, upper = FALSE)
  
  node_size <- count(car_clean, word)
  
  #Filter the word_net size, only pairs with more than 1 occurence and maximum of 50 graph edges
  edges <- edges %>% filter(n > 1)
  edges <- edges[1:50,]
  edges <- na.omit(edges)
  
  #Trimming node_size df to contain the same elements as data.frame edges; to avoid error
  node_size2 <- as.data.frame(base::union(base::intersect(edges$item1, node_size$word), (base::intersect(edges$item2, node_size$word))) )
  colnames(node_size2) <- "word"
  node_size2$word <- as.character(node_size2$word)
  node_size <- inner_join(node_size, node_size2, by = "word")
  
  nodes_size <- edges %>% gather(item, word, item1, item2) %>% group_by(word) %>% summarise(n = sum(n)) %>% rename(Freq = n)
  
  
  
  word_net <- edges %>%
    graph_from_data_frame(vertices = node_size) %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#00436A") +
    geom_node_point(color = "darkslategray4", aes(size = n)) + scale_size(range = c(2,10))+
    scale_fill_discrete(name = "New Legend Title")+
    geom_node_text(aes(label = name), repel = TRUE, size = 5, point.padding = unit(0.2, "lines"), color = "black") +
    theme_void() + 
    theme(axis.text = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.title = element_text(size = 14, face= "bold", colour = "white"),
        legend.text = element_text(color= "black", face = "bold", size = 12),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "snow"))
  

#------Graph2: Ende
 
  output$diagram41 <- renderPlot({
    
    # ggplotly(line_plot, width = 600)
    line_plot
  })
  
  output$diagram42 <- renderPlot({
    word_net
  })
  
  
  valueBoxData <- read.csv(file = "datafiles/Twitter/tweets_per_day.csv", encoding = "UTF-8")
  valueBoxData$created_at <- ymd(valueBoxData$created_at)
  valueBoxData$n <- as.numeric(valueBoxData$n)
  
  output$TrainBox <- renderValueBox({
    
    value <- filter(valueBoxData, domain == "train", 
                    city == displayed_city, created_at >= displayed_date_start, created_at < displayed_date_end) %>% dplyr::select(n) %>% sum()
    
    valueBox(
      value, "Train tweets for selected period", icon = icon("subway", lib = "font-awesome"),
      color = "aqua"
    )
  })
  
  output$TrafficBox <- renderValueBox({
    
    value <- filter(valueBoxData, domain == "traffic", 
                    city == displayed_city, created_at >= displayed_date_start, created_at < displayed_date_end) %>% dplyr::select(n) %>% sum()
    
    
    valueBox(
      value, "Traffic tweets for selected period", icon = icon("car", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  })
  
}

#-- Map
coords <- data.frame("loc"=c("Berlin", "Hamburg", "Frankfurt", "Cologne", "Munich"), 
                     "lng"=c(13.404954, 9.993682, 8.682127, 6.953101, 11.5819806),  
                     "lat"=c(52.520007, 53.551086, 50.110922, 50.935173, 48.1351253))

#Map data preparation


colors <- c("#FF8C00", "#00436A")

train_texts <- dplyr::select(tweets, status_id, text, created_at, city, domain)

# 2. add lat & lng to every tweet 
train_texts$lng <- ifelse(train_texts$city == "berlin", coords[1,2], 
                          ifelse(train_texts$city == "hamburg", coords[2, 2], 
                                 ifelse(train_texts$city == "frankfurt", coords[3,2], 
                                        ifelse(train_texts$city == "koeln", coords[4,2], coords[5,2]))))
train_texts$lat <- ifelse(train_texts$city == "berlin", coords[1,3], 
                          ifelse(train_texts$city == "hamburg", coords[2, 3], 
                                 ifelse(train_texts$city == "frankfurt", coords[3,3], 
                                        ifelse(train_texts$city == "koeln", coords[4,3], coords[5,3]))))


g4miniChart <- function(global){
  
  displayed_period <- as.character(global$period)
  displayed_date_start <- ymd(global$dateG4)
  displayed_date_end <- ymd(global$dateG4) + weeks(1)
  
  #Calculaction of the displayed date range according to both date pickers
  if(displayed_period == "three-days"){displayed_date_end <- as.Date(displayed_date_start) + days(3)} 
  else if(displayed_period == "weekly"){displayed_date_end <- as.Date(displayed_date_start) + weeks(1)} 
  else if (displayed_period == "monthly") {displayed_date_end <- as.Date(displayed_date_start) %m+% months(1)}
  else {displayed_date_end <- as.Date(displayed_date_start) + years(1)}
  
  displayed_city <- tolower(global$location) 
  if(displayed_city == "munich") {
    displayed_city <- "muenchen"
  }
  if(displayed_city == "cologne") {
    displayed_city <- "koeln"
  }
  
  # 3. create data for both map visualizations
  map_train <- filter(train_texts, domain == "train", city == displayed_city, created_at >= displayed_date_start, created_at < displayed_date_end) %>% 
    group_by(lat, lng, domain) %>% count()
  map_traffic <- filter(train_texts, domain == "traffic", city == displayed_city, created_at >= displayed_date_start, created_at < displayed_date_end) %>% 
    group_by(lat, lng, domain) %>% count()
  map_minichart_data <- inner_join(map_train, map_traffic, by = c("lat", "lng")) %>% rename(Train = n.x, Traffic = n.y) %>% ungroup() %>% dplyr::select(lat, lng, Train, Traffic)
  
  
  return(map_minichart_data)
}


g4cluster <- function(global){
  
  displayed_period <- as.character(global$period)
  displayed_date_start <- ymd(global$dateG4)
  displayed_date_end <- ymd(global$dateG4) + weeks(1)
  
  #Calculaction of the displayed date range according to both date pickers
  if(displayed_period == "three-days"){displayed_date_end <- as.Date(displayed_date_start) + days(3)} 
  else if(displayed_period == "weekly"){displayed_date_end <- as.Date(displayed_date_start) + weeks(1)} 
  else if (displayed_period == "monthly") {displayed_date_end <- as.Date(displayed_date_start) %m+% months(1)}
  else {displayed_date_end <- as.Date(displayed_date_start) + years(1)}
  
  displayed_city <- tolower(global$location) 
  if(displayed_city == "munich") {
    displayed_city <- "muenchen"
  }
  if(displayed_city == "cologne") {
    displayed_city <- "koeln"
  }
  map_cluster_data <- filter(train_texts, city == displayed_city, created_at >= displayed_date_start, created_at < displayed_date_end) %>% dplyr::select(domain, lng, lat, text)
  map_cluster_data <- head(map_cluster_data, 10)
  return(map_cluster_data)
}




