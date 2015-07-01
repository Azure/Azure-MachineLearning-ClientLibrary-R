consumeURL = "https://requestresponse001.cloudapp.net/workspaces/bbc91d900c3546b695d6507867fc72ae/services/5cab01b0918c42acb3d32bf597f320f5/execute?api-version=2.0&details=true"
apiKey = "iuB4GWUkRhhXWqa5dhSXJ/hmC9PHfvuyRvP9yb5RJM/F62Bz8wqpGOXpjEB9gQHko02D3J1MHxMhu3j0pHHWcA=="

# Ritika's function
consumeSingleRequest <- function(api_key, URL, columnNames, ...) {
  # Accept SSL certificates issued by public Certificate Authorities
  values = output_list <- lapply(X=list(...), function(x) x)

  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  h = basicTextGatherer()
  hdr = basicHeaderGatherer()

  req = list(
    Inputs = list(
      input1 = list(
        ColumnNames = columnNames,
        Values = values
      )
    )
  )

  body = enc2utf8(toJSON(req))
  print(body)
  print(api_key)
  authz_hdr = paste('Bearer', api_key, sep=' ')
  h$reset()
  curlPerform(url = URL,
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE,
              ssl.verifypeer = FALSE,
              ssl.verifyhost = FALSE
  )

  headers = hdr$value()
  httpStatus = headers["status"]
  if (httpStatus >= 400)
  {
    print(paste("The request failed with status code:", httpStatus, sep=" "))

    # Print the headers - they include the request ID and the timestamp, which are useful for debugging the failure
    print("headers:")
    print(headers)
  }

  result = fromJSON(h$value())
  return(result)
}

consumeSingleRequest(apiKey, consumeURL, list("icol1"), list(0), list(1))

# Default arguments for demoing
defaultUrl = "https://ussouthcentral.services.azureml.net/workspaces/1bbf481194404066a2ee4998a1da2c43/services/da7952281c630641a9bd1a9b1571f65d/score"
defaultKey = "PAhtatK4xlaDB5G3hnDcEwuuYM45GafP3cSdY1PHBBIEQcUW3Ze9fUtYWy4wpbNy5foWGZIMs8FTG/a0EfOzSQ=="
defaultParams = list(
  Id="score00001",
  Instance=list(
    FeatureVector = list(
      "c1" = "4")
    ,
    GlobalParameters = RJSONIO::fromJSON('{}')
  ))

#' Make a call to an API with arguments
#'
#' @param serviceUrl The url of the web app
#' @param key The API key
#' @param toScore the parameters
#' @return The call to the web service at \code{serviceUrl} with arguments \code{toScore}
#' @examples
#' nKey = "JlSp5W+RWf2boTHLwvOvW32j/dDI8d/+ghCb8HTZHKYBl+QkZE46w+ZAxTAdo6U1lXfR6G2SBgnK3/i3VznSww=="
#' nUrl = "https://ussouthcentral.services.azureml.net/workspaces/c01fb89129aa4ef0a19affa7f95ecbbc/services/dadb2d2e626b4e06981dfc6b2b960ebb/execute?api-version=2.0&details=true"
#' nParams = list(
#'   Inputs = list(
#'    "input1" = list(
#'      "ColumnNames" = list("Column 0", "Class", "Sex", "Age", "Freq"),
#'      "Values" = list( list( "0", "value", "value", "value", "0" ),  list( "0", "value", "value", "value", "0" )  )
#'    )                ),
#'  GlobalParameters = fromJSON('{}')
#')
#' predictService(url, key, params)
predictService <- function(serviceUrl=defaultUrl, key=defaultKey, toScore=defaultParams) {

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  h = RCurl::basicTextGatherer()
  hdr = RCurl::basicTextGatherer()

  body = RJSONIO::toJSON(toScore)
  api_key = key
  #api_key = "PAhtatK4xlaDB5G3hnDcEwuuYM45GafP3cSdY1PHBBIEQcUW3Ze9fUtYWy4wpbNy5foWGZIMs8FTG/a0EfOzSQ==" # Replace this with the API key for the web service
  authz_hdr = paste('Bearer', api_key, sep=' ')

  h$reset()
  RCurl::curlPerform(url = serviceUrl,
              httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
              postfields=body,
              writefunction = h$update,
              headerfunction = hdr$update,
              verbose = TRUE
  )

  # TODO: prettify results?
  print("Result:")
  result = h$value()
  print(RJSONIO::fromJSON(result))
}
