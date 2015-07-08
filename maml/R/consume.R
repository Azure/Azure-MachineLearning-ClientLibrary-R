library("RCurl")
library("RJSONIO")
library("data.table")
library("df2json")
library("jsonlite")
library("httr")

# TODO: are all these packages needed? What distinguishes a data.table from a data.frame, and
# why can't rjson or jsonlite (we should consolidate to one of these) handle data.frames to json?
# Removing excess package usage will be more user friendly

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
  #read file into dataframe, convert into matrix and read column by column into list of lists
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
  
  #process batch by batch and make api calls and store responses
  for(i in 1:(length(values))) {
    valuebatch[length(valuebatch) + 1] = values[i]
    if(counter == batchSize || i == (length(values))) {
      temp <- callAPI(api_key, requestURL, columnNames, valuebatch, globalParam, retryDelay)
      if(resultStored != "") {
        resultStored = paste(resultStored,temp,sep='\n')
      } else{
        resultStored = paste(resultStored,temp,sep='')
      }
      
      #write to results file
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
  #store arguments as mega list of lists
  valuesList <- lapply(X=list(...), function(x) x)
  #make api call with components of payload
  result <- callAPI(api_key, requestURL, columnNames, valuesList,  globalParam, retryDelay)
  resultDF <- data.frame(matrix(jsonlite::fromJSON(result)$Results$output1$value$Values))
  colnames(resultDF) <- jsonlite::fromJSON(result)$Results$output1$value$ColumnNames
  return(resultDF)
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
  #format as matrix and parse column by column
  columnNames = colnames(valuesDF)
  matrixdf <- as.matrix(valuesDF)
  rownames(matrixdf) <- NULL
  colnames(matrixdf) <- NULL
  matrixdf <- lapply(seq_len(nrow(matrixdf)), function(row) matrixdf[row,])
  values = matrixdf
  resultStored = ""
  valuebatch = list()
  counter = 1
  
  #process in batches and make API calls in batches
  for(i in 1:(length(values))) {
    valuebatch[length(valuebatch) + 1] = values[i]
    if(counter == batchSize || i == (length(values))) {
      temp <- callAPI(api_key, requestURL, columnNames, valuebatch, globalParam, retryDelay)
      if(resultStored != "") {
        resultStored = paste(resultStored,temp,sep='\n')
      } else{
        resultStored = paste(resultStored,temp,sep='')
      }
      print(sprintf("%i %s %i %s", i,"out of",length(values),"processed"))
      valuebatch = list()
      counter = 0
    }
    counter = counter +1
  }
  resultStored <- jsonlite::fromJSON(resultStored)
  resultDF <- data.frame(matrix(resultStored$Results$output1$value$Values))
  colnames(resultDF) <- resultStored$Results$output1$value$ColumnNames
  return(resultDF)
}


#' This function is a helper that takes in an API key, values and column names to pass to the API and the request URL (OData Endpoint Address).
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the consumeFile function.

callAPI <- function(api_key, requestURL, columnNames, values,  globalParam, retryDelay) {
  httpStatus = 0
  tries = 0
  # limit number of API calls to 3
  for(i in 1:3) {
    #make api calls and prepare payload if httpStatus indicates server error or if 
    if(tries == 0 || httpStatus >= 500) {
      if(httpStatus >= 500) {
        #delay by fixed or specified time if server error
        print(paste("The request failed with status code:", httpStatus, sep=" "))
        print("headers:")
        print(headers)
        print(sprintf("%s %f %s", "Retrying in ",retryDelay," seconds"))
        Sys.sleep(retryDelay)
      }
      tries = tries + 1
      #construct request payload and load RCurl functions
      options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
      h = RCurl::basicTextGatherer()
      hdr = RCurl::basicHeaderGatherer()
      req = list(
        Inputs = list(
          "input1" = list(
            "ColumnNames" = columnNames,
            "Values" = values
          )
        )
        ,GlobalParameters = globalParam
      )
      body = enc2utf8((rjson::toJSON(req)))
      
      #make call to API after constructing request payload
      
      authz_hdr = paste('Bearer', api_key, sep=' ')
      h$reset()
      RCurl::curlPerform(url = requestURL,
                         httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                         postfields=body,
                         writefunction = h$update,
                         headerfunction = hdr$update,
                         verbose = TRUE
                         #                 Parameters below are needed if using test environment, but should not be included for security reasons                  
                         ,ssl.verifypeer=FALSE,
                         ssl.verifyhost = FALSE
      )
      
      headers = hdr$value()
      httpStatus = headers["status"]
      result = h$value()
      formatresult <- jsonlite::toJSON(jsonlite::fromJSON(result), pretty = TRUE)

    }
    #return if successful
    if(httpStatus == 200) {
      return(formatresult)
    } 
    #if user error, print and return error details
    else if ((httpStatus>= 400) && (500 > httpStatus))
    {
      print(paste("The request failed with status code:", httpStatus, sep=" "))
      print("headers:")
      print(headers)
      break
    }
  }
  return(formatresult)
}

discoverSchema <- function(requestURL) {
  # It's a self-signed cert, hence need to ignore host
  httr::set_config(config(ssl_VERIFYHOST=FALSE,ssl_verifyPEER=FALSE), override=TRUE)
  resp <- httr::GET(requestURL)
  
  # will parse automatically
  swagger <- httr::content(resp)
  
  #condensed three steps into one line: Access JSON and then use rjson and json lite in order to structure it as a layered json object
  inputschema = jsonlite::toJSON(jsonlite::fromJSON((RJSONIO::toJSON(swagger$definitions$ExecutionInputs))), pretty = TRUE)
  inputexample <- jsonlite::toJSON(jsonlite::fromJSON((RJSONIO::toJSON(swagger$definitions$ExecutionRequest$example))), pretty = TRUE)
  
  # return both by putting them into a list
  returnList = list(inputschema, inputexample)
  return (returnList)
}