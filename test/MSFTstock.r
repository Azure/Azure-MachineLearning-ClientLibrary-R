### Trying maml and O16N package for Azure ML ####
###==============================================#

#Insructions can be found here:
# https://microsoft.sharepoint.com/teams/CECloudML/_layouts/15/WopiFrame.aspx?sourcedoc=%7b07cb13e8-f6cc-45ae-b930-24cf888e12a9%7d&action=edit&wd=target%28%2F%2FRedmond%20Services%20Documentation.one%7C526ede3c-d252-46d1-b8a0-222b16bffef0%2FO16N%20in%20RStudio%20getting%20started%7Ca5b848b0-7c08-4049-8144-5326e27834f8%2F%29

# My credentials:
#Azure work space ID: 65f07eef16484e018d6df9d87d001d9d
#Primary Authentication token: c9aff3d1c298481b8c402679c1ec8a4a
#Secondary Authencation token: 2596c8ffe4384b9bb2dce1396ce25d4c

library(quantmod)

getSymbols('MSFT')
class(MSFT)
chartSeries(MSFT)
str(MSFT)
plot(MSFT[,1],MSFT[,2])
data = as.data.frame(cbind(MSFT[,4],MSFT[,5]/100000))
plot(data)


model = lm(MSFT.Close~.,data=data)
summary(model)


MSFTpredict <- function(close, volume) {
  return(predict(model, data.frame("MSFT.Close"=close, "MSFT.Volume"=volume)))
}

MSFTonline <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), wsID, wsAuth)
endpoints <- MSFTonline[[2]]
response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("close", "volume"), list(25, 300), list(30, 100))
