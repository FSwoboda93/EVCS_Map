#file from group5: Luftdaten
# function returns a dataframe aggregated by mean for every n rows(step)
# this function is not used any longer but can deliver more variety how to aggregate data.
library(lubridate)


create_avg_data <- function(db, step=2){
  
  num_first <-1
  numrow <- 1+ step
  avg_date <- ymd_hms(c(), tz = "CET")
  avg_p1 <- c()
  new_data <-data.frame("avg_timestamp" =avg_date, "avg_p1" =avg_p1)

  while(numrow <=nrow(db)){
    
    avg_p1 <-  mean(db[num_first:numrow,]$P1)
    avg_date <- mean(db[num_first:numrow,]$timestamp)
    nrow <- data.frame(avg_date, avg_p1)
    new_data <- rbind(new_data, nrow)
    
    num_first <- num_first + step+1
    numrow <- numrow + step+1
    
  }
  #print(new_data)
  return (new_data)
}