# Whose credentials / account should I be using?
testID = "37310abb304e4f56bdb66d279477e0be"
testAuth = "ccfe0f6e9c684345a634bdae0b48c4e9"

webservices = getWebServices(testID, testAuth)
testWS = getWSDetails(testID, testAuth, webservices[[1]]$Id)
endpoints = getEndpoints(testID, testAuth, testWS$Id)
testEP = getEPDetails(testID, testAuth, testWS$Id, endpoints$Name)


test_that("Can discover any endpoints of any web services available starting from workspace ID", {
  expect_equal(length(webservices), 1)
  expect_equal(length(testWS), 7)
  expect_equal(length(endpoints),1)
  expect_equal(length(testEP), 14)
  expect_equal(webservices[[1]]$Id, testWS$Id)
  expect_equal(testWS$Id, endpoints[[1]]$WorkspaceId)
  expect_equal(endpoints[[1]]$WebServiceId, testEP$WebServiceId)
  expect_equal(endpoints[[1]]$Name, testEP$Name)
})

test_that("API location is returned and able to be used immediately", {

})

test_that("Discovery function handle various HTTP error codes and give useful feedback", {

})
