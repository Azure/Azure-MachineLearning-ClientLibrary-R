## Naive Bayes multi-class classifier for iris dataset ##

wsID = "3612640f27234eb7b2b91ac62e8b4a40" #Replace with own workspace ID 
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be" #Replace with own workspace authorization token 

install.packages("e1071")
library("e1071")

dataset <- read.csv(file="iris.csv")

# Train the model
model <- naiveBayes(as.factor(Species) ~., dataset)

# Create prediction function
predictClass <- function(sepalLength, sepalWidth, petalLength, petalWidth) {
  predictDF <- predict(model, data.frame("sepalLength" = sepalLength, "sepalWidth" = sepalWidth, "petalLength" = petalLength, "petalWidth"=petalWidth), type="raw")
  return(colnames(predictDF)[apply(predictDF, 1, which.max)])
}

