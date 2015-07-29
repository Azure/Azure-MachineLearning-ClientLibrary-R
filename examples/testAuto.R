require(testit) || install.packages(testit)
library("testit")

wsID = "3612640f27234eb7b2b91ac62e8b4a40" #Replace with own workspace ID 
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be" #Replace with own workspace authorization token 


########################################################### TITANIC TESTS ###########################################################

#Test titanic model
titanicWebService <- publishWebService("predictTitanicSurvival", "TestTitanic", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)

# Use getEndpoints method
# endpoints <- getEndpoints(wsID, wsAuth, titanicWebService[[1]]["Id"], internalURL)
# Access results from return value of publish method
titanicEndpoints <- titanicWebService[[2]]

#May take long since servers are getting warmed up
titanicConsumeSingleRows <- consumeDataTable(titanicEndpoints[[1]]["PrimaryKey"], titanicEndpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))

# consume with inputs as dataframe
# creating test data.frame
titanicDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c(8,20, 30), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
TitanicConsumeDF <- consumeDataframe(titanicWebService[[2]][[1]]$PrimaryKey, titanicWebService[[2]][[1]]$ApiLocation, titanicDF)

############################################################# IRIS TESTS #############################################################
#Publish iris prediction function
irisWebService <- publishWebService("predictClass", "TestIris", list("sepalLength"="float", "sepalWidth"="float", "petalLength"="float", "petalWidth"="float"), list("class"="int"), wsID, wsAuth)

#Test discovery method getEndpoints
irisEndpoints <- irisWebService[[2]]

#Consume single request
irisConsumeSingleRows <- consumeDataTable(irisEndpoints[[1]]$PrimaryKey, irisEndpoints[[1]]$ApiLocation, list("sepalLength", "sepalWidth", "petalLength", "petalWidth"), list(5, 5, 4, 3), list(4.5, 6.5, 4.5, 2))

#Consume data frame
irisDF <- data.frame("sepalLength"=c(1,2,3), "sepalWidth"=c(4,5,6), "petalLength"=c(7,8,9), "petalWidth"=c(10,11,12))
irisConsumeDF <- consumeDataframe(irisWebService[[2]][[1]]$PrimaryKey, paste(irisWebService[[2]][[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), irisDF)

############################################################# MSFTPREDICT TESTS #############################################################

#Publish MSFT prediction function
msftWebService <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), wsID, wsAuth)

# Discover endpoints 
msftEndpoints <- msftWebService[[2]]

# 
msftConsumeSingleRows <- consumeDataTable(msftEndpoints[[1]]["PrimaryKey"], paste(msftEndpoints[[1]]$ApiLocation, "/execute?api-version=2.0&details=true",sep=""), list("close", "volume"), list(25, 300), list(30, 100))

msftDF <- data.frame("close"=c(107,208,300), "volume"=c(400,569,665))
msftConsumeDF <- consumeDataframe(msftWebService[[2]][[1]]$PrimaryKey, paste(msftWebService[[2]][[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), msftDF)


############################################################# BREAST CANCER TESTS #############################################################

# Publish web service
onlineCluster <- publishWebService("getCluster", "kMeansCancer", list("age"="int","mp"="int", "tSize"="int", "invNodes"="int",
                                                                      "nodeCaps"="int", "DegMalig"="int", "Breast"="int", "BreastQuad"="int",
                                                                      "Irradiat"="int"), list("cluster"="int"), wsID, wsAuth)
# Consume web service
endpoints <- onlineCluster[[2]]
responseDF <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation,
                               list("age", "mp", "tSize", "invNodes", "nodeCaps", "DegMalig", "Breast", "BreastQuad", "Irradiat"),
                               list(7, 2, 3, 4, 2, 3, 2, 1, 1))

############################################################# BIKE COUNT TESTS #############################################################

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
