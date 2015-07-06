# Azure-MachineLearning-ClientLibrary-R

The preview of Azure Machine Learning R client package lets you access your Azure ML webservices from your local RStudio environment.

This is a technology preview. The APIs exposed by the library and the REST endpoints it connects to are subject to change.


Publishing a web service

FUNCTIONS:
publishWebService <- function(functionName, serviceName, inputSchema, outputSchema, wkID, authToken)

Discovering web services

FUNCTIONS:
getEndpoints <- function(wkID, authToken, wsID, url=epURL)
getEPDetails <- function(wkID, authToken, wsID, epID, url=epURLdet)
getWebServices <- function(wkID, authToken, url=wsURL)
getWSDetails <- function(wkID, authToken, wsID, url=wsURLdet)

Consuming a web service

FUNCTIONS:
consumeFile <- function(api_key, requestURL, infileName, globalParam = "", outfileName = "results.txt", batchSize = 250, retryDelay = 0.3)
consumeLists <- function(api_key, requestURL, columnNames, ..., globalParam="", retryDelay = 0.3)
consumeDataframe <- function(api_key, requestURL, valuesDF, globalParam="", batchSize = 250, retryDelay = 0.3)
