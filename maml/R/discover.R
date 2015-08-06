# API URLs ----------------------------------------------------------------

prodURL = "https://management-tm.azureml.net"

# Functions ---------------------------------------------------------------

#' Abstraction for making GET requests.
#'
#' Framework for making GET requests to the Azure management APIs.
#'
#' @param tUrl the API URL
#' @param authToken the authentication token
#'
#' @return the response as a named list
#'
#' @family discovery
#' @keywords internal
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



#' Get Available Web Services.
#'
#' Get a list of webservices available to the Microsoft Azure Machine Learning workspace specified by the workspace ID.
#'
#' @export
#'
#' @param wkID workspace ID
#' @param authToken primary authorization token
#' @param url the API url to make the call to, by default hits the Azure management API
#'
#' @return Returns a list of lists, where each web service is represented as a nested named list with the following fields:
#'
#' \itemize{
#'   \item Id
#'   \item Name
#'   \item Description
#'   \item CreationTime
#'   \item WorkspaceId
#'   \item DefaultEndpointName
#' }
#'
#' @family discovery
#'
#' @examples
#' \dontrun{
#' services = getWebServices("wsID", "authToken")
#' serviceID = services[[1]]["Id"]
#' }
getWebServices <- function(wkID, authToken, url=prodURL) {
  response = getFramework(sprintf(paste(url,"/workspaces/%s/webservices",sep=""), wkID), authToken)
  if (!is.list(response)) {
    stop("No web services found", call. = TRUE)
  }
  return(response)
}



#' Get Web Service Details.
#'
#' Get detailed information about a specific Microsoft Azure Machine Learning web service specified by the web service ID.
#'
#' @export
#'
#' @inheritParams getWebServices
#' @param wsID the web service ID
#'
#' @return Returns a list with the following fields:
#'
#' \itemize{
#'   \item Id
#'   \item Name
#'   \item Description
#'   \item CreationTime
#'   \item WorkspaceId
#'   \item DefaultEndpointName
#' }
#'
#' @family discovery
getWSDetails <- function(wkID, authToken, wsID, url=prodURL) {
  return(getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s", sep=""), wkID, wsID), authToken))
}



#' Get Web Service Endpoints.
#'
#' Get the API endpoints that belong to a Microsoft Azure Machine Learning web service.
#'
#' @export
#'
#' @inheritParams getWSDetails
#'
#' @return Returns a list of lists, where each endpoint is represented
#' as a nested named list with the following fields:
#'
#' \itemize{
#'  \item Name
#'  \item Description
#'  \item CreationTime
#'  \item WorkspaceId
#'  \item WebServiceId
#'  \item HelpLocation
#'  \item PrimaryKey
#'  \item SecondaryKey
#'  \item ApiLocation
#'  \item Version
#'  \item MaxConcurrentCalls
#'  \item DiagnosticsTraceLevel
#'  \item ThrottleLevel
#'  }
#'
#' @family discovery
#'
#' @examples
#' \dontrun{
#' endpoints <- getEndpoints("wkId", "authToken", "wsID")
#' apiURL <- endpoints[[1]]$HelpLocation
#' pKey <- endpoints[[1]]$PrimaryKey
#' }
getEndpoints <- function(wkID, authToken, wsID, url=prodURL) {
  response <- getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints", sep=""), wkID, wsID), authToken)
  # for convenience because by default the repsonse doesn't include the full API location
  for (i in 1:length(response)) {
    response[[i]]$ApiLocation <- paste(response[[i]]$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  }
  return(response)
}



#' Get Endpoint Details.
#'
#' Get detailed information about a specific endpoint for a web service specified by the web service ID
#'
#' @export
#'
#' @inheritParams getWSDetails
#' @param epName endpoint name
#'
#' @return Returns a list with the following fields:
#'
#' \itemize{
#'  \item Name
#'  \item Description
#'  \item CreationTime
#'  \item WorkspaceId
#'  \item WebServiceId
#'  \item HelpLocation
#'  \item PrimaryKey
#'  \item SecondaryKey
#'  \item ApiLocation
#'  \item Version
#'  \item MaxConcurrentCalls
#'  \item DiagnosticsTraceLevel
#'  \item ThrottleLevel
#'  }
#'
#' @family discovery
getEPDetails <- function(wkID, authToken, wsID, epName, url=prodURL) {
  sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints/%s", sep=""), wkID, wsID, epName)
  endpoint <- getFramework(sprintf(paste(url, "/workspaces/%s/webservices/%s/endpoints/%s", sep=""), wkID, wsID, epName), authToken)
  # for convenience because by default the repsonse doesn't include the full API location
  endpoint$ApiLocation <- paste(endpoint$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  return(endpoint)
}
