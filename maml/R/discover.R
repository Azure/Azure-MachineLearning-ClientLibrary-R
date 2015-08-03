#' @docType package
#' @name discover
#' The discovery code allows the user to retrieve a list of the web services available in
#' their workspace given that they provide the workspace ID and the authorization token
#' (both of which can be found in settings on the AzureML webpage). A user can also get
#' detailed information about a specific web service, retrieve its endpoints, and the
#' details of a specific endpoint.



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

internalURL = "https://management.azureml-int.net"
prodURL = "https://management-tm.azureml.net"

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

  # Error handle response not long enough (no webservices)
  if (h$value() == "") {
    return(-1)
  }
  response = RJSONIO::fromJSON(h$value())
  # Error handling
  if ('error' %in% names(response)) {
    stop(response$error)
  }

  return(response)
}



#############################################################
#' @title Get Web Services
#' @export
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
getWebServices <- function(wkID, authToken, url=prodURL) {
  response = getFramework(sprintf(paste(url,"/workspaces/%s/webservices",sep=""), wkID), authToken)
  if (!is.list(response)) {
    stop("No web services found", call. = TRUE)
  }
  return(response)
}



#############################################################
#' @title Get Workspace Details
#' @export
#' @description Get detailed information about a specific webservice
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @return Returns a named list representing the web service
#' with the following fields:
#' "Id", "Name", "Description", "CreationTime", "WorkspaceId", "DefaultEndpointName"
#' @examples
#' services = getWebServices("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456")
#############################################################
getWSDetails <- function(wkID, authToken, wsID, url=prodURL) {
  return(getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s", sep=""), wkID, wsID), authToken))
}



#############################################################
#' @title Get Endpoints
#' @export
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
#' endpoints = getEndpoints("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456")
#############################################################
getEndpoints <- function(wkID, authToken, wsID, url=prodURL) {
  response <- getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints", sep=""), wkID, wsID), authToken)
  # for convenience because by default the repsonse doesn't include the full API location
  for (i in 1:length(response)) {
    response[[i]]$ApiLocation <- paste(response[[i]]$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  }
  return(response)
}



#############################################################
#' @title get Endpoint Details
#' @export
#' @description Get the details on a specific endpoint
#'
#' @param wkID The workspace ID
#' @param authToken The primary authorization token
#' @param wsID The webservice ID
#' @param epName The endpoint name
#' @return Returns a named list representing the endpoint with the following fields:
#' "Name", "Description", "CreationTime", "WorkspaceId", "WebServiceId",
#' "HelpLocation", "PrimaryKey", "SecondaryKey", "ApiLocation", "Version",
#' "MaxConcurrentCalls", "DiagnosticsTraceLevel", "ThrottleLevel"
#' @examples
#' defaultEP = getEPDetails("abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "abcdefghijklmnopqrstuvwxyz123456", "default")
#############################################################
getEPDetails <- function(wkID, authToken, wsID, epName, url=prodURL) {
  sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints/%s", sep=""), wkID, wsID, epName)
  endpoint <- getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints/%s", sep=""), wkID, wsID, epName), authToken)
  # for convenience because by default the repsonse doesn't include the full API location
  endpoint$ApiLocation <- paste(endpoint$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  return(endpoint)
}
