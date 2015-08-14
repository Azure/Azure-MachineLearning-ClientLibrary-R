test_that("discoverSchema returns help page information", {
  expect_that(length(schema), equals(4))
  expect_that(schema$requestUrl, is_equivalent_to("https://ussouthcentral.services.azureml.net/workspaces/f5e8e9bc4eed4034b78567449cfca779/services/b4caf0f0ebfd451bbc187741894e213b/execute?api-version=2.0&format=swagger"))
  expect_that(schema$columnNames, is_equivalent_to(list("Survived", "PassengerClass", "Gender", "Age", "SiblingSpouse", "ParentChild", "FarePrice", "PortEmbarkation")))
  expect_that(schema$sampleInput, is_equivalent_to(list("Survived"=1, "PassengerClass"=1, "Gender"="male", "Age"=1, "SiblingSpouse"=1, "ParentChild"=1, "FarePrice"=1, "PortEmbarkation"="C")))
})



test_that("consumeLists, non-R web function", {
  response <- consumeLists("qh1cUv695D29eQkRV+zor8VTOWcEoxVTjMZWA4H7X0o8NEAUHZM13CHjOoqRNRGzXgQPxHMw6607YKI0vbhRxA==",
                           schema$requestUrl, schema$sampleInput)
  expect_that(as.numeric(response[1,1]), equals(1))
  expect_that(as.numeric(response[1,2]), equals(.875))
})



test_that("consumeDataframe, non-R web service", {
  df <- data.frame("Survived"="1", "PassengerClass"="1", "Gender"="male", "Age"=1, "SiblingSpouse"=1, "ParentChild"=1, "FarePrice"=1, "PortEmbarkation"="C")
  response <- consumeDataframe("qh1cUv695D29eQkRV+zor8VTOWcEoxVTjMZWA4H7X0o8NEAUHZM13CHjOoqRNRGzXgQPxHMw6607YKI0vbhRxA==",
                               schema$requestUrl, df)
  expect_that(as.numeric(response[1,1]), equals(1))
  expect_that(as.numeric(response[1,2]), equals(.875))
})
