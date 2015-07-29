## Random forest model for bike dataset ##

# You can use the setwd() command to change your working directory. Examples below
#setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/examples")
#setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/test")

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification

wsID = "3612640f27234eb7b2b91ac62e8b4a40" #Replace with own workspace ID 
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be" #Replace with own workspace authorization token 

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