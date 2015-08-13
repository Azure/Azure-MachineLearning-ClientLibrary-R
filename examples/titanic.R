## GBM model for Titanic dataset ##
require(gbm) || install.packages("gbm")
library(gbm)

# Load data
# Make sure you are in the directory containing these files
test <- read.csv(file="titanicTest.csv")
train <- read.csv(file="titanicTrain.csv")

# You can use the setwd() command to change your working directory.

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification
wsID = "" # Insert workspace ID
wsAuth = "" # Insert workspace authorization token


# Preprocessing
#y variable
survived <- train$Survived
#id
passengerId <- test$PassengerId
#remove from the training sample
train = train[,-2]
end_trn = nrow(train)
#combine the two sets
train <- rbind(train, test)
#Age replace with mean
train$Age[is.na(train$Age)] <- 30
end = nrow(train)
#remove columns
train = train[,c(-1,-3,-8,-10,-11)]
head(train)

# Train model

set.seed(123)
pr=0
tr=0

n.models = 5
ntrees=2000

for(i in 1:n.models){
  GBM.model2 = gbm.fit(
    x=train[1:end_trn,], y = survived,
    distribution= "gaussian",
    n.trees = ntrees,
    shrinkage = 0.01,
    interaction.depth = 25,
    n.minobsinnode = 5,
    verbose = TRUE)
  #test set prediction
  pr1 = predict(object=GBM.model2,newdata=train[(end_trn+1):end,], ntrees)
  #training set prediction
  tr1 = predict(object = GBM.model2,newdata=train[1:end_trn,], ntrees)
  pr = pr+pr1
  tr = tr+tr1
}
pr = pr/n.models
tr = tr/n.models
summary(GBM.model2)

# Create prediction function
predictTitanicSurvival <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(predict.gbm(object=GBM.model2, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000))
}

#Test titanic model
titanicWebService <- publishWebService("predictTitanicSurvival", "TestTitanic", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)

# Use getEndpoints method
# endpoints <- getEndpoints(wsID, wsAuth, titanicWebService[[1]]["Id"], internalURL)
# Access results from return value of publish method
titanicEndpoints <- titanicWebService[[2]]

#May take long since servers are getting warmed up
titanicConsumeSingleRows <- consumeLists(titanicEndpoints[[1]]["PrimaryKey"], titanicEndpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))

# consume with inputs as dataframe
# creating test data.frame
titanicDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c(8,20, 30), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
TitanicConsumeDF <- consumeDataframe(titanicWebService[[2]][[1]]$PrimaryKey, titanicWebService[[2]][[1]]$ApiLocation, titanicDF)
