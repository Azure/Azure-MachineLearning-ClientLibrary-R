#############################################################
# API URLs
#############################################################
wsURL = "https://management-tm.azureml.net/workspaces/%s/webservices"
wsURLdet = "https://management-tm.azureml.net/workspaces/%s/webservices/%s"
epURL = "https://management-tm.azureml.net/workspaces/%s/webservices/%s/endpoints"
epURLdet = "https://management-tm.azureml.net/workspaces/%s/webservices/%s/endpoints/%s"

# remove in real version
testURL = "https://hiteshsm.cloudapp.net/workspaces/%s/webservices/%s/endpoints"

#############################################################
# Framework for making an HTTP request to the URL specified
# returns a list of lists, so that the elements can be accessed
# via double bracket notation, e.g. results[[1]]
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
              verbose = TRUE,
              ssl.verifyhost = FALSE # REMOVE FOR PRODUCTION
  )

  # Print results
  return(RJSONIO::fromJSON(h$value()))
}



#############################################################
#' Get a list of webservices available to a workspace
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
#' Get detailed information about a specific webservice
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @return Returns a named list representing the web service
#' with the following fields:
#' "Id", "Name", "Description", "CreationTime", "WorkspaceId", "DefaultEndpointName"
#' @examples
#' DELETE TOKENS IN REAL VERSION
#' service = getWSDetails("c01fb89129aa4ef0a19affa7f95ecbbc", "523709d06661441bbf129d68f84cd6a4", "6a46d1f2a5e6406b8b1a5c563bf1cd10")
#' serviceID = service["Id"]
#############################################################
getWSDetails <- function(wkID, authToken, wsID, url=wsURLdet) {
  return(getFramework(sprintf(url, wkID, wsID), authToken))
}



#############################################################
#' Get the endpoints that are part of a web service
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
#' DELETE TOKENS IN REAL VERSION
#' endpoints = getEndpoints("c01fb89129aa4ef0a19affa7f95ecbbc", "523709d06661441bbf129d68f84cd6a4", "6a46d1f2a5e6406b8b1a5c563bf1cd10")
#' defaultEP = endpoints[[1]]
#############################################################
getEndpoints <- function(wkID, authToken, wsID, url=epURL) {
  return(getFramework(sprintf(url, wkID, wsID), authToken))
}



#############################################################
#' Get the details on a specific endpoint
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
#' DELETE TOKENS IN REAL VERSION
#' defaultEP = getEPDetails("c01fb89129aa4ef0a19affa7f95ecbbc", "523709d06661441bbf129d68f84cd6a4", "6a46d1f2a5e6406b8b1a5c563bf1cd10", "default")
#############################################################
getEPDetails <- function(wkID, authToken, wsID, epID, url=epURLdet) {
  return(getFramework(sprintf(url, wkID, wsID, epID), authToken))
}
