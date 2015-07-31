## Random forest model for bike dataset ##

# You can use the setwd() command to change your working directory. Examples below
#setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/examples")
#setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/test")

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification

wsID = "" #Replace with own workspace ID
wsAuth = "" #Replace with own workspace authorization token

require(randomForest) || install.packages("randomForest")
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
