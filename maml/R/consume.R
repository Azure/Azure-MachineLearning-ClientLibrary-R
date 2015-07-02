library("RCurl")
library("rjson")
library("data.table")
library("df2json")


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
  values = as.list(result$Results$output1$value$Values)
  return(list(values, result))
}


#' This function takes in an API key, file name and the request URL (OData Endpoint Address).
#' It calls a helper function that sends requests to the server to the server in the appropriate format.
#' It processes requests in batches and stores the responses in order of batches in an array. It returns the output columns along with the scored probablities, and stores the result in a text file.
#' @param api key must be entered as the first parameter, and must be a string
#' @param requestURL must be entered as the third parameter, and must be a string
#' @param infileName the name of the file that is being scored
#' @param globalParam global parameters entered as a string, default value is ""
#' @param outfileName the name of the file to write results to, entered as a string, with a default value of "results.txt"
#' @param batchSize of each batch, which is optional, but 100 by default
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list

consumeFile <- function(api_key, requestURL, infileName, globalParam = "", outfileName = "results.txt", batchSize = 250, retryDelay = 0.3) {
  if (missing(api_key)) {
    stop("Need to specify API key")
  }
  if (missing(infileName)) {
    stop("Need to specify file to be scored")
  }
  if (missing(requestURL)) {
    stop("Need to specify request URL")
  }
  dataFrame = read.csv(infileName,check.names=FALSE)
  columnNames = colnames(dataFrame)
  matrixdf <- as.matrix(dataFrame)
  rownames(matrixdf) <- NULL
  colnames(matrixdf) <- NULL
  matrixdf <- lapply(seq_len(nrow(matrixdf)), function(row) matrixdf[row,])
  values = matrixdf
  resultStored = ""
  valuebatch = list()
  counter = 1
  fileConn <-file(outfileName,"w")

  for(i in 1:(length(values))) {
    valuebatch[length(valuebatch) + 1] = values[i]
    if(counter == batchSize || i == (length(values))) {
        temp <- callAPI(api_key, requestURL, columnNames, valuebatch, globalParam, retryDelay)
        resultStored = paste(resultStored,temp,sep=', ')
        write(temp, fileConn,append = TRUE)
        print(sprintf("%i %s %i %s", i,"out of",length(values),"processed"))
        valuebatch = list()
        counter = 0
    }
    counter = counter +1
  }
  close(fileConn)
  return(resultStored)
}




#' This function takes in an API key, the request URL (OData Endpoint Address), the column names and multiple requests
#' It scores the experiment with the requests stored in a list of lists, and sends it to the server in the appropriate format.
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the user. It returns the output column(s) along with the scored probablities!
#' @param api key must be entered as the first parameter, and must be a string
#' @param requestURL must be entered as the third parameter, and must be a string
#' @param columnNames entered as a list
#' @param ... each parameter must be a request in the format of a list that contains a row of values corresponsing to the column names provided
#' @param globalParam global parameters entered as a string, default value is ""
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list

consumeLists <- function(api_key, requestURL, columnNames, ..., globalParam="", retryDelay = 0.3) {
  if (missing(api_key)) {
    stop("Need to specify API key")
  }

  if (missing(requestURL)) {
    stop("Need to specify request URL")
  }
  if (missing(columnNames)) {
    stop("Need to specify column names")
  }
  if(missing(globalParam)) {
    globalParam = ""
  }
  valuesList = list(...)

  callAPI(api_key, requestURL, columnNames, valuesList,  globalParam, retryDelay)
}


#' This function takes in an API key, the request URL (OData Endpoint Address), the column names and multiple requests
#' It scores the experiment with the requests stored in a list of lists, and sends it to the server in the appropriate format.
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the user. It returns the output column(s) along with the scored probablities!
#' @param api key must be entered as the first parameter, and must be a string
#' @param requestURL must be entered as the third parameter, and must be a string
#' @param valuesDF the name of the data frame that is being scored
#' @param globalParam global parameters entered as a string, default value is ""
#' @param batchSize of each batch, which is optional, but 100 by default
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list

consumeDataframe <- function(api_key, requestURL, valuesDF, globalParam="", batchSize = 250, retryDelay = 0.3) {
  if (missing(api_key)) {
    stop("Need to specify API key")
  }

  if (missing(requestURL)) {
    stop("Need to specify request URL")
  }
  if (missing(valuesDF)) {
    stop("Need to specify dataframe to be scored")
  }
  columnNames = colnames(valuesDF)
  matrixdf <- as.matrix(valuesDF)
  rownames(matrixdf) <- NULL
  colnames(matrixdf) <- NULL
  matrixdf <- lapply(seq_len(nrow(matrixdf)), function(row) matrixdf[row,])
  values = matrixdf
  resultStored = ""
  valuebatch = list()
  counter = 1

  for(i in 1:(length(values))) {
    valuebatch[length(valuebatch) + 1] = values[i]
    if(counter == batchSize || i == (length(values))) {
      temp <- callAPI(api_key, requestURL, columnNames, valuebatch, globalParam, retryDelay)
      resultStored = paste(resultStored,temp,sep=', ')
      print(sprintf("%i %s %i %s", i,"out of",length(values),"processed"))
      valuebatch = list()
      counter = 0
    }
    counter = counter +1
  }
  return(resultStored)}


#' This function is a helper that takes in an API key, values and column names to pass to the API and the request URL (OData Endpoint Address).
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the consumeFile function.

callAPI <- function(api_key, requestURL, columnNames, values,  globalParam, retryDelay) {
  httpStatus = 0
  tries = 0
  for(i in 1:3) {
    if(tries == 0 || httpStatus >= 500) {
      if(httpStatus >= 500) {
        print(paste("The request failed with status code:", httpStatus, sep=" "))
        print("headers:")
        print(headers)
        print(sprintf("%s %i %s", "Retrying in ",retryDelay," seconds"))
        Sys.sleep(retryDelay)
      }
      tries = tries + 1
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
        ,GlobalParameters = globalParam
      )

      body = enc2utf8(toJSON(req))
      authz_hdr = paste('Bearer', api_key, sep=' ')
      h$reset()
      curlPerform(url = requestURL,
                  httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                  postfields=body,
                  writefunction = h$update,
                  headerfunction = hdr$update,
                  verbose = TRUE
      )

      headers = hdr$value()
      httpStatus = headers["status"]
      result = h$value()
    }
    if(httpStatus == 200) {
      return(result)
    }
    else if ((httpStatus>= 400) && (500 > httpStatus))
    {
      print(paste("The request failed with status code:", httpStatus, sep=" "))
      print("headers:")
      print(headers)
      break
    }
  }
  return(result)
}

