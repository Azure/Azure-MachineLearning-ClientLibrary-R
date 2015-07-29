## GBM model for Titanic dataset ##
install.packages("gbm")
library(gbm)

# Credentialing
wsID = "3612640f27234eb7b2b91ac62e8b4a40" #Replace with own workspace ID 
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be" #Replace with own workspace authorization token 

# Load data
test <- read.csv(file="titanicTest.csv")
train <- read.csv(file="titanicTrain.csv")

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