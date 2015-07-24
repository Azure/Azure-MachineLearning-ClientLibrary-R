## Linear model for MSFT stock dataset ##

setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"

library(quantmod)
getSymbols('MSFT')
class(MSFT)
chartSeries(MSFT)
str(MSFT)
plot(MSFT[,1],MSFT[,2])
data = as.data.frame(cbind(MSFT[,4],MSFT[,5]/100000))

# Train model
model = lm(MSFT.Close~.,data=data)
summary(model)

# Create prediction function
MSFTpredict <- function(close, volume) {
  return(predict(model, data.frame("MSFT.Close"=close, "MSFT.Volume"=volume)))
}

# Publish web service
MSFTonline <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), wsID, wsAuth)

# Consume web service
endpoints <- MSFTonline[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("close", "volume"), list(25, 300), list(30, 100))
