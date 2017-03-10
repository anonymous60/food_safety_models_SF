#*******
# 01- Data Extract, Transform, and load(ETL)
#*******
workdir = "~/Documents/Stanford/SF_FoodSafety"


dir = getwd()
if (dir != workdir){
  setwd(workdir)
}
source("src/generate_Rscore_data.R")
source("src/download_weather_history_all.R")
source("src/generate_weather_data.R")
source("src/weather_passed3day_avg.R")

generate_Rscore_data()

wunderground.key = "6bd64342112a2676"
#download_weather_history_all(wunderground.key)
generate_weather_data()

weather <- readRDS("data_source/weather_Data.Rds")
weather_3day <- weather_passed3day_avg(weather)


#*******
# 01-Test: If the above function calls succeed, 
# you should see the following Rds files under the data_source directory
#*******

test = readRDS("data_source/SFopenData_Restaurant_Scores.Rds")
print(dim(test))
#[1] 53729    10
test = readRDS("data_source/Weather_Data.Rds")
print(dim(test))
#[1] 1097    5
test = readRDS("data_source/weather_historyData.Rds")
print(dim(test))
#[1]   2 1097
test = readRDS("data_source/weather_passed3day_avg.Rds")
print(dim(test))
#[1] 1094    5