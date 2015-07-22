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
head(pr)
head(tr)
summary(GBM.model2)

#round the preidctions
pr = round(pr)
tr = round(tr)

#compare results
head(tr, n=20)
head(survived, n = 20)

#in sample classification accuracy
1 - sum(abs(survived - tr)) / nrow(train)

predictTitanicSurvival <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(predict.gbm(object=GBM.model2, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000))
}

submission = data.frame(PassengerId = passengerId, survived = pr)
write.csv(submission, file = "gbm submission.csv", row.names = FALSE)
