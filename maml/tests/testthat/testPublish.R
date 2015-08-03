#test_that("GetFunctionString gets a function source code with no quotes or newlines", {
#  add <- function (x, y) {
#    print("This will add x and y")
#    return(x + y)
#  }

#  expect_equal(getFunctionString(add), "function (x, y) \r\n{\r\n    print(\"This will add x and y\")\r\n    return(x + y)\r\n}")
#})


#test_that("packDependencies handles recursive packaging", {
#  skip_on_cran()
#  require("e1071") || install.packages("e1071")
#  library("e1071")
#  require(quantmod) || install.packages(quantmod)
#  library(quantmod)


#  # Train the model
#  model <- naiveBayes(as.factor(Species) ~., dataset)
#})


#test_that("publishWebService returns a working web service", {
#  skip_on_cran()
#  testID = ""
#  testAuth = ""
#  require(quantmod) || install.packages(quantmod)
#  library(quantmod)

#  getSymbols('MSFT')
#  data = as.data.frame(cbind(MSFT[,4],MSFT[,5]/100000))
#  model = lm(MSFT.Close~.,data=data)

#  MSFTpredict <- function(close, volume) {
#    return(predict(model, data.frame("MSFT.Close"=close, "MSFT.Volume"=volume)))
#  }

#  msftWebService <- publishWebService("MSFTpredict", "MSFTdemo", list("close"="float", "volume"="float"), list("number"="float"), testID, testAuth)
#  msftEndpoints <- msftWebService[[2]]
#  response <- consumeDataTable(msftEndpoints[[1]]["PrimaryKey"], msftEndpoints[[1]]$ApiLocation, list("close", "volume"), list(25, 300), list(30, 100))

#  expect_equal(as.numeric(response[1,1]), 1)
#  expect_equal(as.numeric(response[2,1]), 1)
#})


#test_that("publishWebService handles bad input correctly", {
#  add <- function (x, y) {
#    print("This will add x and y")
#    return(x + y)
#  }

#  expect_error(publishWebService("add", "addTest", list(), list(), testID, testAuth), "Input schema does not contain the proper input. You provided 0 inputs and 2 were expected")
#  expect_error(publishWebService("add", "addTest", list("x"="foo", "y"="bar"), list("z"="int"), testID, testAuth), "data type \"foo\" not supported")
#})


#test_that("publishWebService handles various errors correctly", {
#  add <- function (x, y) {
#    print("This will add x and y")
#    return(x + y)
#  }

#  expect_error(publishWebService("add", "add", list("x"="float", "y"="float"), list("z"="float"), "foo", "bar"), "InvalidWorkspaceIdInvalid workspace ID provided. Verify the workspace ID is correct and try again.")
#})


#test_that("updateWebService correctly updates a web service", {
#  skip_on_cran()
#  testID = ""
#  testAuth = ""
#  add1 <- function (x) {
#    return(x + 1)
#  }
#  expect_equal(consumeDataTable("yoDjX5h4Cm/fjjWRlcfN9NWP8Zsob9nfKblI9D6di8lfZ6ecU1CClMwA4ITMhkVWp4IQjGqPvRudhVX8uSS5Cw==", "https://ussouthcentral.services.azureml-int.net/workspaces/3612640f27234eb7b2b91ac62e8b4a40/services/04f9321f08e548299e79ca79b0bd8517/execute?api-version=2.0&details=true/execute?api-version=2.0&details=true",
#                   list("x"), list(1))[[1]], as.factor(2))


#  add2 <- function (x) {
#    return(x + 2)
#  }

#  updateWebService("add2", "addTest", "b48b51fa364c11e5bc9595fc9bdb7ae6", list("x"="float"), list("y"="float"), testID, testAuth)
#  expect_equal(consumeDataTable("yoDjX5h4Cm/fjjWRlcfN9NWP8Zsob9nfKblI9D6di8lfZ6ecU1CClMwA4ITMhkVWp4IQjGqPvRudhVX8uSS5Cw==", "https://ussouthcentral.services.azureml-int.net/workspaces/3612640f27234eb7b2b91ac62e8b4a40/services/04f9321f08e548299e79ca79b0bd8517/execute?api-version=2.0&details=true/execute?api-version=2.0&details=true",
#                                list("x"), list(1))[[1]], as.factor(3))
#  updateWebService("add1", "addTest","b48b51fa364c11e5bc9595fc9bdb7ae6", list("x"="float"), list("y"="float"), testID, testAuth)
#})

