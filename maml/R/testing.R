dataframe <- read.csv("forestfires.csv")
testConsumeDF <- consumeDataframe("Hv5kVkT5Lt6stj+jqGVE836j2E2VTkhPo3Sb0h/J7ulxWeU/MMNqE5O08shGqLwuaZRIfZmPdlrQC+2IANxp6w==","https://requestresponse001.cloudapp.net/workspaces/0f2adea9926a4e7c9a636a39f2026fa0/services/1a11ca46456a428a92f57638d20a72f9/execute?api-version=2.0&details=true",dataframe)


testing <- publishWebService("add", "e2eTest2", list("icol1"="int"), list("ocol1"="int"), wsID, auth)
endpoint <- getEndpointsT(wsID, auth, testing["Id"])
consumeSingleRequest(endpoint[[1]]["PrimaryKey"], paste(endpoint[[1]]["ApiLocation"],"/execute?api-version=2.0&details=true",sep=""), list("icol1"), list(1))

wrapper <- "inputDF <- maml.mapInputPort(1)\r\noutputDF <- matrix(ncol = %s, nrow = nrow(inputDF))\r\ncolnames(outputDF) <- %s\r\noutputDF <- data.frame(outputDF)\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(paste(\"src\", file, sep=\"/\"), lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(strsplit(file, \"\\\\.\")[[1]][[1]], character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\n  for (i in 1:nrow(inputDF)) {\r\n    outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n  }\r\nmaml.mapOutputPort(\"outputDF\")"

# No zip file / opening of dependencies
"inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(matrix(ncol = %s, nrow = nrow(inputDF)))\r\naction <- %s\r\n  for (i in 1:nrow(inputDF)) {\r\n    outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n  }\r\nmaml.mapOutputPort(\"outputDF\")"

# viewing contents of /src/
"outputDF <- data.frame(list.files(\"src\"))\r\nmaml.mapOutputPort(\"outputDF\")"
"inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(list.files(\"src\")\r\nmaml.mapOutputPort(\"outputDF\")"

#Testing consumption functions

testconsumeSingleDT <- consumeDataTable("Hv5kVkT5Lt6stj+jqGVE836j2E2VTkhPo3Sb0h/J7ulxWeU/MMNqE5O08shGqLwuaZRIfZmPdlrQC+2IANxp6w==","https://requestresponse001.cloudapp.net/workspaces/0f2adea9926a4e7c9a636a39f2026fa0/services/1a11ca46456a428a92f57638d20a72f9/execute?api-version=2.0&details=true",list( "X","Y","month","day","FFMC","DMC","DC","ISI","temp","RH","wind","rain","area"),list("0","0","jan","mon","0","0","0","0","0","0","0","0","0"))
testConsumeFile <- consumeFile("Hv5kVkT5Lt6stj+jqGVE836j2E2VTkhPo3Sb0h/J7ulxWeU/MMNqE5O08shGqLwuaZRIfZmPdlrQC+2IANxp6w==","https://requestresponse001.cloudapp.net/workspaces/0f2adea9926a4e7c9a636a39f2026fa0/services/1a11ca46456a428a92f57638d20a72f9/execute?api-version=2.0&details=true&format=swagger","forestfires.csv")
testConsumeDF <- consumeDataframe("Hv5kVkT5Lt6stj+jqGVE836j2E2VTkhPo3Sb0h/J7ulxWeU/MMNqE5O08shGqLwuaZRIfZmPdlrQC+2IANxp6w==","https://requestresponse001.cloudapp.net/workspaces/0f2adea9926a4e7c9a636a39f2026fa0/services/1a11ca46456a428a92f57638d20a72f9/execute?api-version=2.0&details=true&format=swagger",dataframe)
testDiscoverSchema <- discoverSchema("7e8f135f31274b7eac419bd056875c03", "a5b003e52c924d16a2e38ade45dd0154")
testconsumeSingle <- consumeLists("Hv5kVkT5Lt6stj+jqGVE836j2E2VTkhPo3Sb0h/J7ulxWeU/MMNqE5O08shGqLwuaZRIfZmPdlrQC+2IANxp6w==","https://requestresponse001.cloudapp.net/workspaces/0f2adea9926a4e7c9a636a39f2026fa0/services/1a11ca46456a428a92f57638d20a72f9/execute?api-version=2.0&details=true&format=swagger",list( "X" = 0,"Y" = 0,"month" = "jan","day" = "mon","FFMC" = 0,"DMC" = 0,"DC" = 0,"ISI" = 0,"temp" = 0,"RH" = 0,"wind"=0,"rain" = 0,"area" = 0))


##################################################################################
# WRAPPER TESTING
# Consider: assert statements (stopifnot), error handling
# wrap in a function that will return a string with the proper function
##################################################################################
# get the input
inputDF <- maml.mapInputPort(1)

# initialize an empty output dataframe of desired dimensions
#outputDF <- data.frame(matrix(ncol = "%s", nrow = nrow(inputDF)))
outputDF <- matrix(ncol = "%s", nrow=nrow(inputDF))
colnames(outputDF) <- "%s"
outputDF <- data.frame(outputDF)

for (file in list.files("src")) {
  if (file == "%s") {
    load("src/%s")
    # assert that dependencies exists?
    # NOTE: depedencies object comes from packDependencies(), maybe something more unique to avoid collisions?
    for (item in names(dependencies)) {
      assign(item, dependencies[[item]])
    }
  }
  else {
    # if the package isn't installed on Azure already, install it and its dependencies
    # need to recursively grab dependencies
    if (!(file %in% installed.packages()[,"Package"])) {
      install.packages(paste("src", file, sep="/"), lib= ".", repos=NULL, verbose=TRUE)
    }
    # load the package
    library(strsplit(file, "\\.")[[1]][[1]], character.only=TRUE)
  }
}

# user function
action <-
  
  # apply function to every row
  for (i in 1:nrow(inputDF)) {
    outputDF[i,] <- do.call("action", as.list(inputDF[i,]))
  }

# return output
maml.mapOutputPort("outputDF")

# test function
add <- function(x) {
  print(findGlobals(add))
  return(add2(x))
}

add2 <- function(x) {
  print(toJSON(list("test")))
  return(x+a[[1]])
}

testService <- publishWebService("add", "addTest", list("out"="float"), list("in"="float"), wsID, wsAuth)
testEndpoints <- testService[[2]]
response <- consumeDataTable(testEndpoints[[1]]["PrimaryKey"], paste(testEndpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("out"), list("1"), list("2"), list("3"))

testModel <- function(Pclass, Sex, Age, SibSp, Parch, Fare) {
  class(GBM.model)
  return(list(Pclass, Sex, Age, SibSp, Parch, Fare))
}
TitanicTest <- publishWebService("testModel", "TestingModel7-13", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), wsID, wsAuth)
testEndpoints <- TitanicTest[[2]]
response3 <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", "20", "2", "0", 8.50))
