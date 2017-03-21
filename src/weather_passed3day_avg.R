#
#' Returns the 3-day average weather
#'
#' This function take the weather data for each date,
#' and returns the data frame of passed three days average
#'
#' @param weather A data frame of weather data
#' @param savedata Optionally save the output to the file
#'                 "weather_passed3day_avg.rda"
#' @return A data frame of 3-day average weather data
#' @author 
weather_passed3day_avg <- function(weather,
                                   savedata = FALSE){
  weather <- as.data.frame(weather)
  nrows <- nrow(weather)
  weather <- weather[order(weather$Date, decreasing = TRUE),]

  avg_3day<- as.data.frame(zoo::rollapply(weather[,2:5],3,mean))
  avg_3day<- avg_3day[-1,]
  avg_3day$Inspection_Date <- weather$Date[1:(nrows-3)]
  if (savedata)
    save(avg_3day, file = "weather_passed3day_avg.rda")
  return(avg_3day)
}
