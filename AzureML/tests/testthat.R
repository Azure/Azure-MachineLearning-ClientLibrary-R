library(testthat)
library(AzureML)

add <- function(x,y) {
  return(x+y)
}

schema <- discoverSchema("https://studio.azureml.net/apihelp/workspaces/f5e8e9bc4eed4034b78567449cfca779/webservices/d42667a354e34a3f98888ba86300fc2f/endpoints/b4caf0f0ebfd451bbc187741894e213b/score")

schema$sampleInput$Gender <- "male"
schema$sampleInput$PortEmbarkation <- "C"

test_check("AzureML")
