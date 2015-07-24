## Random forest model for bike dataset ##

setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

library(randomForest)
BikeShare <- read.csv(file="bikes.csv")

# Train the model
rf.bike <- randomForest(cnt ~ xformHr + temp +
                          monthCount + hum + dayWeek +
                          mnth + isWorking + workTime,
                        data = BikeShare, ntree = 500,
                        importance = TRUE, nodesize = 25)

# Create prediction function
predictBikeCount <- function(dateTime, month, hr, hldy, wkdy, weather, temp, hum,
                             wndspd, casual, registered, cnt, isWorking, mthCnt, dayWeek, wrkTime, xformHr) {
  return(predict(rf.bike, newdata=data.frame("dteday"=dateTime, "mnth"=month, "hr"=hr, "holiday"=hldy, "workingday"=wkdy, "weathersit"=weather,
                                      "temp"=temp, "hum"=hum, "windspeed"=wndspd, "casual"=casual, "registered"=registered, "cnt"=cnt,
                                      "isWorking"=isWorking, "monthCount"=mthCnt, "dayWeek"=dayWeek, "workTime"=wrkTime, "xformHr"=xformHr)))
}

# Publish web service
bikeCount <- publishWebService("predictBikeCount", "bikeCount", list("dateTime"="date-time", "month"="int", "hr"="int", "hldy"="int", "wkdy"="int", "weather"="int",
                                                                     "temp"="int", "hum"="int", "wndspd"="int", "casual"="int", "registered"="int", "cnt"="int",
                                                                     "isWorking"="int", "mthCnt"="int", "dayWeek"="int", "wrkTime"="int", "xformHr"="int"),
                               list("count"="int"), wsID, wsAuth)

# Consume web service
endpoints <- bikeCount[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], endpoints[[1]]["ApiLocation"],
                             list("dateTime", "month", "hr", "hldy", "wkdy", "weather",
                              "temp", "hum", "wndspd", "casual", "registered", "cnt",
                              "isWorking", "mthCnt", "dayWeek", "wrkTime", "xformHr"),
                             list("1/1/12 12:00:00", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
