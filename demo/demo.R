#####################################################################################################
# TITANIC DEMO
#####################################################################################################

# IMPORTANT: need to compile all of consume, discover, publish functions before running this demo
setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/demo")
myID = "bbc91d900c3546b695d6507867fc72ae"
myAuth = "ffc4b8d52c494e9eb42726b77112be88"
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
tr = tr/n.models
head(pr)
head(tr)
summary(GBM.model)

# create a function to make predictions using the trained model
predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(predict.gbm(object=GBM.model, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000))
}

# Sample local call
predictTitanic(1, "male", "20", "2", "0", "8.50")

# Publish the function
result <- publishWebService("predictTitanic", "TitanicDemo7-7", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), myID, myAuth)

# Currently response is a list of three things: 
#   new web service details, default endpoint details, specific consumption function
# Rename the consumption function
consumeTitanic <- result[[3]]

# Use the new function
response <- consumeTitanic(list("1", "male", "20", "2", "0", "8.50"))

# Convert the response into values