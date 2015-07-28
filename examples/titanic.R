## GBM model for Titanic dataset ##

# You can use the setwd() command to change your working directory. Examples below
#setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/examples")
#setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/test")

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

# Load data
# Make sure you are in the directory containing these files
test <- read.csv(file="test.csv")
train <- read.csv(file="train.csv")

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
require(gbm) || install.packages("gbm")
library(gbm)
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

# Publish web service
TitanicService <- publishWebService("predictTitanic", "TitanicDemo", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)

# Consume web service
endpoints <- getEndpoints(wsID, wsAuth, TitanicService[[1]]$Id, internalURL)
# Alternatively,
endpoints <- TitanicService[[2]]
# First, consume with inputs as a list
response <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list("1", "male", "20", "2", "0", "8.50"), list("1", "female", "20", "1", "0", "8.50"))
response2 <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list("2", "male", "50", "1", "0", "8.50"), list("2", "female", "50", "1", "0", "8.50"))

# consume with inputs as dataframe
# creating test data.frame
demoDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c("8","20", "30"), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
responseDF <- consumeDataframe(TitanicService[[2]][[1]]$PrimaryKey, TitanicService[[2]][[1]]$ApiLocation, demoDF)
