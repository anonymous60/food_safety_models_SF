#*********
# Using the wunderground web API to retrieve historical average temperature 
# for specified dates, download the data into "weather_historyData.Rds"
#
# API calls require the Key ID:  6bd64342112a2676
#
# Notice: the total number of fetches of data vie the API is limited perday by Key ID
# Perferably only run this function ONCE.
#*********


download_weather_history_all <- function(wunderground.key){
  if(!file.exists("data_source/weather_historyData.Rds") &&
     file.exists("data_source/SFopenData_Restaurant_Scores.Rds")){
    
      require(stringr)
      Rscore.table <- readRDS("data_source/SFopenData_Restaurant_Scores.Rds")
      YMD <- unique(Rscore.table$inspection_date) 
      YMD <- seq(range(YMD)[1],range(YMD)[2], by="days")
      YMD <- str_replace_all(as.character(YMD),"-", "") 
      APIs <- paste("http://api.wunderground.com/api/", wunderground.key, "/history_", YMD,"/q/CA/San_Francisco.json", sep = "" )
      history.table <- sapply(APIs, fromJSON)
      saveRDS(history.table, file = "data_source/weather_historyData.Rds")
      
  }else {
    
    print("SFopenData_Restaurant_Scores.Rds cannot be found or weather_historyData.Rds already exists!")
    
  }
}