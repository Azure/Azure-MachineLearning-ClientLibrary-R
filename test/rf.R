setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
library(randomForest)
dataset <- read.csv(file="rfData.csv")

model <- randomForest(Count ~ ., dataset)

predictCount <- function(season, holiday, workingday, weather, temp, atemp, humidity, windspeed, hour, weekday, month, year) {
  return(predict(model, data.frame("season" = season, "holiday" = holiday, "workingday" = workingday, "weather"=weather,
                                   "temp"=temp, "atemp"=atemp, "humidity"=humidity, "windspeed"=windspeed, "hour"=hour, 
                                  "weekday"=weekday,"month"=month, "year"=year)))
}

irisService <- publishWebService("predictClass", "irisService7-21", list("sepalLength"="float", "sepalWidth"="float", "petalLength"="float", "petalWidth"="float"), list("class"="int"), wsID, wsAuth)
endpoints <- irisService[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("sepalLength", "sepalWidth", "petalLength", "petalWidth"), list(5, 5, 4, 3), list(4.5, 6.5, 4.5, 2))

