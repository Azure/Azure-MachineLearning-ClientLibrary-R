## Linear model for MSFT stock dataset ##

# You can use the setwd() command to change your working directory. Examples below

# Currently using identification for an account on studio.azureml-int.net
# If you would like to see the web services published, please create an account there
# and substitute in your identification

wsID = "" # Insert workspace ID
wsAuth = "" # Insert workspace authorization token

require(quantmod) || install.packages(quantmod)
library(quantmod)

getSymbols('MSFT')
class(MSFT)
chartSeries(MSFT)
str(MSFT)

#plot(MSFT[,1],MSFT[,2])
data = as.data.frame(cbind(MSFT[,4],MSFT[,5]/100000))

# Train model
model = lm(MSFT.Close~.,data=data)
summary(model)

# Create prediction function
MSFTpredict <- function(close, volume) {
  return(predict(model, data.frame("MSFT.Close"=close, "MSFT.Volume"=volume)))
}

#Publish MSFT prediction function
msftWebService <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), wsID, wsAuth)

# Discover endpoints
msftEndpoints <- msftWebService[[2]]

#
msftConsumeSingleRows <- consumeDataTable(msftEndpoints[[1]]["PrimaryKey"], msftEndpoints[[1]]$ApiLocation, list("close", "volume"), list(25, 300), list(30, 100))

msftDF <- data.frame("close"=c(107,208,300), "volume"=c(400,569,665))
msftConsumeDF <- consumeDataframe(msftWebService[[2]][[1]]$PrimaryKey, msftWebService[[2]][[1]]$ApiLocation, msftDF)
