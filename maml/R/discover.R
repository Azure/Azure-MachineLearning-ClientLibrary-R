#############################################################
# API URLs
#############################################################
wsURL = "https://management-tm.azureml.net/workspaces/%s/webservices"
wsURLdet = "https://management-tm.azureml.net/workspaces/%s/webservices/%s"
epURL = "https://management-tm.azureml.net/workspaces/%s/webservices/%s/endpoints"
epURLdet = "https://management-tm.azureml.net/workspaces/%s/webservices/%s/endpoints/%s"

# remove in real version
testURL = "https://hiteshsm.cloudapp.net/workspaces/%s/webservices/%s/endpoints"
internalURL = "https://management.azureml-int.net/workspaces/%s/webservices/%s/endpoints"

#############################################################
#' @title Get FrameWork
#' @description
#' Framework for making an HTTP request to the URL specified
#' returns a list of lists, so that the elements can be accessed
#' via double bracket notation
#' @param tUrl The URL from the published web service
#' @param authToken The authentication token for the AzureML account being used
#' @return prints the framework
#' @examples
#' results[[1]]
#############################################################
getFramework <- function(tUrl, authToken) {
  # Collectors for API response
  h = RCurl::basicTextGatherer()
  hdr = RCurl::basicTextGatherer()

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Craft request header and execute
  auth = paste('Bearer', authToken, sep=' ')
  h$reset()
  RCurl::curlPerform(url = tUrl,
              httpheader=c('Authorization' = auth, 'Content-Type' = "application/json", 'Accept' = "application/json"),
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE)

  # Print results
  return(RJSONIO::fromJSON(h$value()))
}



#############################################################
#' @title Get Web Services
#' @description Get a list of webservices available to a workspace
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @return Returns a list of lists, where each web service is represented
#' as a nested named list with the following fields:
#' "Id", "Name", "Description", "CreationTime", "WorkspaceId", "DefaultEndpointName"
#' @examples
#' DELETE TOKENS IN REAL VERSION
#' services = getWebServices("c01fb89129aa4ef0a19affa7f95ecbbc", "523709d06661441bbf129d68f84cd6a4")
#' serviceID = services[[1]]["Id"]
#############################################################
getWebServices <- function(wkID, authToken, url=wsURL) {
  return(getFramework(sprintf(url, wkID), authToken))
}



#############################################################
#' @title Get Workspace Details
#' @description Get detailed information about a specific webservice
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @return Returns a named list representing the web service
#' with the following fields:
#' "Id", "Name", "Description", "CreationTime", "WorkspaceId", "DefaultEndpointName"
#' @examples
#' << Please note that you will need to retrieve all of the signature details from your AzureML account >>
#' services = getWebServices("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456")
#############################################################
getWSDetails <- function(wkID, authToken, wsID, url=wsURLdet) {
  return(getFramework(sprintf(url, wkID, wsID), authToken))
}



#############################################################
#' @title Get Endpoints
#' @description Get the endpoints that are part of a web service
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @return Returns a list of lists, where each endpoint is represented
#' as a nested named list with the following fields:
#' "Name", "Description", "CreationTime", "WorkspaceId", "WebServiceId",
#' "HelpLocation", "PrimaryKey", "SecondaryKey", "ApiLocation", "Version",
#' "MaxConcurrentCalls", "DiagnosticsTraceLevel", "ThrottleLevel"
#' @examples
#' << Please note that you will need to retrieve all of the signature details from your AzureML account >>
#' endpoints = getEndpoints("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456")
#############################################################
getEndpoints <- function(wkID, authToken, wsID, url=epURL) {
  return(getFramework(sprintf(url, wkID, wsID), authToken))
}



#############################################################
#' @title get EndPoint Details
#' @description Get the details on a specific endpoint
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @param epID The endpoint name
#' @return Returns a named list representing the endpoint with the following fields:
#' "Name", "Description", "CreationTime", "WorkspaceId", "WebServiceId",
#' "HelpLocation", "PrimaryKey", "SecondaryKey", "ApiLocation", "Version",
#' "MaxConcurrentCalls", "DiagnosticsTraceLevel", "ThrottleLevel"
#' @examples
#' << Please note that you will need to retrieve all of the signature details from your AzureML account >>
#' defaultEP = getEPDetails("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "default")
#############################################################
getEPDetails <- function(wkID, authToken, wsID, epID, url=epURLdet) {
  return(getFramework(sprintf(url, wkID, wsID, epID), authToken))
}
