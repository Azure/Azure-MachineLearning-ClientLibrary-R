library(e1071)

setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
# t-alewa on studio internal
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

dataset <- read.csv(file="iris.csv")

# Azure code
#features <- get.feature.columns(dataset)
#labels   <- as.factor(get.label.column(dataset))
#train.data <- data.frame(features, labels)
#feature.names <- get.feature.column.names(dataset)
#names(train.data) <- c(feature.names, "Class")
#model <- naiveBayes(Class ~ ., train.data)
#probabilities <- predict(model, dataset, type="raw")[,2]
#classes <- as.factor(as.numeric(probabilities >= 0.5))
#classes <- as.factor(predict(model, dataset))
#scores <- data.frame(classes, probabilities)

model <- naiveBayes(as.factor(Species) ~., dataset)

predictClass <- function(sepalLength, sepalWidth, petalLength, petalWidth) {
  predictDF <- predict(model, data.frame("sepalLength" = sepalLength, "sepalWidth" = sepalWidth, "petalLength" = petalLength, "petalWidth"=petalWidth), type="raw")
  return(colnames(predictDF)[apply(predictDF, 1, which.max)])
}

irisService <- publishWebService("predictClass", "irisService7-21", list("sepalLength"="float", "sepalWidth"="float", "petalLength"="float", "petalWidth"="float"), list("class"="int"), wsID, wsAuth)
endpoints <- irisService[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("sepalLength", "sepalWidth", "petalLength", "petalWidth"), list(5, 5, 4, 3), list(4.5, 6.5, 4.5, 2))
