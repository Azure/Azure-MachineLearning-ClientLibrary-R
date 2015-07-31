## Naive Bayes multi-class classifier for iris dataset ##

# You can use the setwd() command to change your working directory. Examples below
#setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/examples")
#setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/test")

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification
wsID = "" # Insert workspace ID
wsAuth = "" # Insert workspace authorization token

require("e1071") || install.packages("e1071")
library("e1071")

dataset <- read.csv(file="iris.csv")

# Train the model
model <- naiveBayes(as.factor(Species) ~., dataset)

# Create prediction function
predictClass <- function(sepalLength, sepalWidth, petalLength, petalWidth) {
  predictDF <- predict(model, data.frame("sepalLength" = sepalLength, "sepalWidth" = sepalWidth, "petalLength" = petalLength, "petalWidth"=petalWidth), type="raw")
  return(colnames(predictDF)[apply(predictDF, 1, which.max)])
}

#Publish iris prediction function
irisWebService <- publishWebService("predictClass", "TestIris", list("sepalLength"="float", "sepalWidth"="float", "petalLength"="float", "petalWidth"="float"), list("class"="int"), wsID, wsAuth)

#Test discovery method getEndpoints
irisEndpoints <- irisWebService[[2]]

#Consume single request
irisConsumeSingleRows <- consumeDataTable(irisEndpoints[[1]]$PrimaryKey, irisEndpoints[[1]]$ApiLocation, list("sepalLength", "sepalWidth", "petalLength", "petalWidth"), list(5, 5, 4, 3), list(4.5, 6.5, 4.5, 2))

#Consume data frame
irisDF <- data.frame("sepalLength"=c(1,2,3), "sepalWidth"=c(4,5,6), "petalLength"=c(7,8,9), "petalWidth"=c(10,11,12))
irisConsumeDF <- consumeDataframe(irisWebService[[2]][[1]]$PrimaryKey, irisWebService[[2]][[1]]$ApiLocation, irisDF)
