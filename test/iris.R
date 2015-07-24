## Naive Bayes multi-class classifier for iris dataset ##

setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
#setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/test")
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

library("e1071")
dataset <- read.csv(file="iris.csv")

# Train the model
model <- naiveBayes(as.factor(Species) ~., dataset)

# Create prediction function
predictClass <- function(sepalLength, sepalWidth, petalLength, petalWidth) {
  predictDF <- predict(model, data.frame("sepalLength" = sepalLength, "sepalWidth" = sepalWidth, "petalLength" = petalLength, "petalWidth"=petalWidth), type="raw")
  return(colnames(predictDF)[apply(predictDF, 1, which.max)])
}

# Publish web service
irisService <- publishWebService("predictClass", "irisService7-21", list("sepalLength"="float", "sepalWidth"="float", "petalLength"="float", "petalWidth"="float"), list("class"="int"), wsID, wsAuth)

# Consume web service
endpoints <- irisService[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("sepalLength", "sepalWidth", "petalLength", "petalWidth"), list(5, 5, 4, 3), list(4.5, 6.5, 4.5, 2))
