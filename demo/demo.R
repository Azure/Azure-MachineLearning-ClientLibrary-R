#####################################################################################################
# TITANIC DEMO
#####################################################################################################

# IMPORTANT: need to compile all of consume, discover, publish functions before running this demo
# setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/demo")
setwd("C://Users/t-ritra/Documents/Github/Azure-MachineLearning-ClientLibrary-R/demo")


# test server
wsID = "bbc91d900c3546b695d6507867fc72ae"
wsAuth = "ffc4b8d52c494e9eb42726b77112be88"

# internal server
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

# 
test <- read.csv(file="test.csv")
train <- read.csv(file="train.csv")

head(test)
head(train)

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

library(gbm)
set.seed(123)
pr=0
tr=0

n.models = 5
ntrees=2000

# train a generalized boosted regression model
for(i in 1:n.models){
  GBM.model = gbm.fit(
    x=train[1:end_trn,], y = survived,
    distribution= "gaussian",
    n.trees = ntrees,
    shrinkage = 0.01,
    interaction.depth = 25,
    n.minobsinnode = 5,
    verbose = TRUE)
  #test set prediction
  pr1 = predict.gbm(object=GBM.model,newdata=train[(end_trn+1):end,], ntrees)
  #training set prediction
  tr1 = predict.gbm(object = GBM.model,newdata=train[1:end_trn,], ntrees)
  pr = pr+pr1
  tr = tr+tr1
}
pr = pr/n.models
tr = tr/n.modelsti
head(pr)
head(tr)
summary(GBM.model)

pr = round(pr)
tr = round(tr)

# create a function to make predictions using the trained model
predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(predict.gbm(object=GBM.model, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000))
}

# Sample local call
predictTitanic(1, "male", "20", "2", "0", "8.50")



# Publish the function
# Go to https://metaanalytics001.cloudapp.net/Home/ViewWorkspace/bbc91d900c3546b695d6507867fc72ae?#Workspace/WebServiceGroups/listWebServiceGroups
# to see published function
TitanicService <- publishWebService("predictTitanic", "TitanicDemo", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)
# Currently response is a list of two things: 
#   new web service details, default endpoint details (using discovery functions)



# Discover the endpoints
# Go to help page
endpoints <- getEndpoints(wsID, wsAuth, TitanicService[[1]]["Id"], internalURL)
# Alternatively,
endpoints <- TitanicService[[2]]



# Consume the new webservice
# First, consume with inputs as a list
# Slow initially as it makes the connection
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list("1", "male", "20", "1", "0", "8.50"), list("1", "female", "20", "1", "0", "8.50"))
# Subsequent calls are faster as connection is left open
response2 <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list("2", "male", "50", "1", "0", "8.50"), list("2", "female", "50", "1", "0", "8.50"))

# consume with inputs as dataframe
# creating test data.frame
demoDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c("8","20", "30"), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
responseDF <- consumeDataframe(TitanicService[[2]][[1]]$PrimaryKey, paste(TitanicService[[2]][[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), demoDF)

