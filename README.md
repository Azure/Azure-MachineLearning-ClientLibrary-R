# Azure-MachineLearning-ClientLibrary-R

This package provides an interface to access web services on Microsoft Azure Machine Learning (MAML) from your local R environment. There are three main functionalities:
- publish: define a custom function or train a model and publish it to MAML
- discover: browse web services that are available to your workspace
- consume: use available web service from R in a variety of convenient formats

This is a technology preview. The APIs used by the package are still subject to change. Please send any bugs or comments you have to the maintainers listed.

## Installing the package

Currently the package lives only on this GitHub repo. You can install the package and the dependencies via the following lines in R:

`install.packages(c("RCurl", "RJSONIO", "uuid", "jsonlite", "codetools", "base64enc", "httr", "data.table", "df2json", "rjson", "devtools"), repos = "http://cran.r-project.org")
`devtools::install_github("Azure-MachineLearning-ClientLibrary-R", "Azure", subdir="maml")

Also, you will need to install [R tools](https://cran.r-project.org/bin/windows/Rtools/) and make sure that a zipper is included in your PATH variable (instructions [here](http://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127))

We plan on releasing the package to CRAN eventually.


## Using the package

Currently the APIs we are using are only deployed internally, so please go [here](studio.azureml-int.net) and create an account. After logging in, under the "Settings" tab, copy and paste your workspace ID from the "Name" sub-tab into your R console. From the "Authorization Tokens" sub-tab, copy your primary authorization token into your R console. You will need this information to access all package functionality.

We expect to migrate to the production version of Azure within a few weeks.


## Publishing a web service

The primary functionality implemented by this package is the capability to publish a model from R to Azure Machine Learning without having to copy and paste your R script to the Machine Learning Studio UI. Publishing a function is now a simple one line function call.

`publishWebService(functionName, serviceName, inputSchema, outputSchema, wkID, authToken)

The publish function takes in the name of the function to be published as a string, the name to be displayed on Azure, your workspace ID, and your authorization token. The function also requires the input and output schemas of the function to be published, which is a list of the format

`list("arg1"=<type>, "arg2"=<type>, ...)

The publish function is can take in an arbitrary function, using arbitrary packages and models.

The publish function will return a lists of lists. The first list contains the details of the web service. The second list contains a list of the endpoints of the web service. Please refer to the example for how to programmatically use the return values to consume the new web service.

You are also able to update your function with one line. Note that this also requires passing the input and output schemas of the function.

`updateWebService(functionName, webServiceID, inputSchema, outputScema, wkID, authToken)

The return value is the same as that of publishWebService()


## Discovering web services

This package allows you to start with a workspace ID and discover all web service available, or start with a web service ID and discover all service endpoints and any information needed to access the endpoint. Service can then be consumed directly in R or used on another platform, e.g. Visual Studio or Excel.

While we currently using internal APIs, in order to discover your web service published through this package, please use the optional parameters "url=internalURL". Otherwise, for discovery in the production environment, simply leave that parameter out.

`getWebServices(workspaceID, authToken, url=internalURL)
`getWSDetails(wkID, authToken, webserviceID, url)
`getEndpoints(wID, authToken, wsID, url)
`getEPDetails(wkID, authToken, wsID, endpointName, url)


## Consuming a web service

The package includes a number of convenience functions to consume a web service via a number of different input formats.

`consumeFile(api_key, requestURL, infileName, globalParam = "", outfileName = "results.txt", batchSize = 250, retryDelay = 0.3)
`consumeLists(api_key, requestURL, columnNames, ..., globalParam="", retryDelay = 0.3)
`consumeDataframe(api_key, requestURL, valuesDF, globalParam="", batchSize = 250, retryDelay = 0.3)


## Example

```
# End to end demo using Titanic survival classifier
# Import data
test <- read.csv(file="test.csv")
train <- read.csv(file="train.csv")

# Data wrangling
survived <- train$Survived
passengerId <- test$PassengerId
train = train[,-2]
end_trn = nrow(train)
train <- rbind(train, test)
train$Age[is.na(train$Age)] <- 30
end = nrow(train)
train = train[,c(-1,-3,-8,-10,-11)]

# Train a gbm model
library(gbm)
set.seed(123)
pr=0
tr=0
n.models = 5
ntrees=2000
for(i in 1:n.models){
  GBM.model = gbm.fit(
    x=train[1:end_trn,], y = survived,
    distribution= "gaussian",
    n.trees = ntrees,
    shrinkage = 0.01,
    interaction.depth = 25,
    n.minobsinnode = 5,
    verbose = TRUE)
}
# define a function to make predictions using the trained model
predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare) {
  return(round(predict.gbm(object=GBM.model, newdata=data.frame("Pclass"=Pclass, "Sex"=Sex, "Age"=Age, "SibSp"=SibSp, "Parch"=Parch, "Fare"=Fare), 2000)))
}

# Sample local call
predictTitanic(1, "male", 20, 2, 0, 8.50)

# Publish the function
TitanicService <- publishWebService("predictTitanic", "TitanicDemoR", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)

# Discover the endpoints
endpoints <- getEndpoints(wsID, wsAuth, TitanicService[[1]]["Id"], internalURL)
# Alternatively,
endpoints <- TitanicService[[2]]

# Consume the new web service
# First, consume with inputs as a list
response <- consumeDataTable(endpoints[[1]]$PrimaryKey, endpoints[[1]]$ApiLocation, list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))
# Next, consume with inputs as dataframe
demoDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c(8,20, 30), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
responseDF <- consumeDataframe(TitanicService[[2]][[1]]$PrimaryKey, paste(TitanicService[[2]][[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), demoDF)
```

Further examples can be found in the demo folder of the repository.