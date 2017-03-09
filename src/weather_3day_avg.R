weather_3day_avg <- function(weather){
  
  require(data.table)
  # str(weather)
  weather <- as.data.frame(weather)
  nr <- nrow(weather)
  weather <- weather[order(-as.numeric(weather$Date)), ]
  threeDay <- weather[2:(nr-2), colnames(weather) != "Date"] + 
    weather[3:(nr - 1), colnames(weather) != "Date"] +
    weather[4:(nr - 0), colnames(weather) != "Date"] 
  threeDay <- threeDay / 3
  threeDay$Date <- weather$Date[1:(nr-3)]
  
  threeDay <- as.data.table(threeDay)
  threeDay[ , date := as.IDate(Date, format="%y-%m-%d")]
  setnames(threeDay, 'Date', "Inspection_Date")
  
  threeDay <- threeDay[, .SD, keyby=Inspection_Date]
  saveRDS(threeDay, file = "data_source/weather_3day.Rds")
  return(threeDay)
}

