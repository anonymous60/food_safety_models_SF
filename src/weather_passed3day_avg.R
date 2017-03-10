weather_passed3day_avg <- function(weather){
## This function take the weather data for each date, 
## and returns the data frame of passed three days average 
  weather <- as.data.frame(weather)
  nrows <- nrow(weather)
  weather <- weather[order(weather$Date, decreasing = TRUE),]
  
  avg_3day<- as.data.frame(rollapply(weather[,2:5],3,mean))
  avg_3day<- avg_3day[-1,]
  avg_3day$Inspection_Date <- weather$Date[1:(nrows-3)]
  saveRDS(avg_3day, file = "data_source/weather_passed3day_avg.Rds")
  return(avg_3day)
  
}

