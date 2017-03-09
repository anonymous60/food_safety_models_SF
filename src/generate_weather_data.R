#********
# Generate the weather data table according to the weather_historyData.Rds,
# and store it as Weather_Data.Rds 
#********


generate_weather_data <- function(){
  if(file.exists("data_source/weather_historyData.Rds") && 
     !file.exists("data_source/weather_Data.Rds")){
    
        temp = readRDS("data_source/weather_historyData.Rds")
        yy_mm_dd = apply(temp, 2, function(x) x[2][[1]]$date["pretty"])
        yy_mm_dd = unlist(yy_mm_dd,recursive = TRUE,use.names = FALSE)
        yy_mm_dd = as.Date(yy_mm_dd,format='%B %d, %Y')
        maxtemp_F = apply(temp, 2, function(x) x[2][[1]]$dailysummary$maxtempi)
        meanwindspdm = apply(temp, 2, function(x) x[2][[1]]$dailysummary$meanwindspdm)
        maxhumidity = apply(temp, 2, function(x) x[2][[1]]$dailysummary$maxhumidity)
        precipi = apply(temp, 2, function(x) x[2][[1]]$dailysummary$precipi)
        weather_data = tibble(Date = yy_mm_dd, 
                              maxtemp_F = as.numeric(maxtemp_F), 
                              meanwindspdm = as.numeric(meanwindspdm), 
                              maxhumidity = as.numeric(maxhumidity), 
                              precipi = as.numeric(precipi))
        saveRDS(weather_data, file = "data_source/weather_Data.Rds")
  }else{
    print("weather_historyData.Rds cannot be found or weather_Data.Rds already exists!")
  }
}