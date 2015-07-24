#' @docType package
#' @name consume
#' Before consuming a function, the user can call the discoverSchema function using the request URL and
#' view the input schema as well as an example of the input to the web service.
#' While consuming a web service, the user currently has the option to score either a CSV file, data frame or
#' individual requests. With either option, the user simply has to make a single-line function call and the
#' scored probabilities are returned to the user in a data frame. There are three functions that the user can
#' choose from. All 3 functions take in the API key and request URL. The consumeLists function takes in a variable
#' number of requests entered in data table format. The option to enter the requests in key-value format is currently
#' being added. The consumeFile function takes in the file name of the file to score, while the consumeDataFrame
#' function takes in a data frame to score. Users also have the option of varying global parameters, delay time
#' before retrying a server call in case of a server error, batch size of each request sent to the server and name
#' of file that results are outputted to. Hence, the user can consume a web service from AzureML regardless of the
#' format in which their data is stored in a single-line function.

# packages to include
library("RCurl")
library("rjson")
library("data.table")
library("df2json")
library("jsonlite")
library("httr")

# change scored prob

#############################################################
#' @title Consume File
#' @description
#' This function takes in an API key, file name and the request URL (OData Endpoint Address).
#' It calls a helper function that sends requests to the server to the server in the appropriate format.
#' It processes requests in batches and stores the responses in order of batches in an array. It returns the output columns along with the scored probablities, and stores the result in a text file.
#' @param string api_key - API key must be entered as the first parameter
#' @param string requestURL - must be entered as the third parameter
#' @param string infileName - The name of the file that is being scored
#' @param string globalParam - global parameters, default value is ""
#' @param string outfileName - The name of the file to write results to, has a default value of "results.txt"
#' @param int batchSize of each batch, which is optional, but 100 by default
#' @param int retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list
#' @examples
#' # First, consume a file
#' # example file name will be text.txt
#' response <- consumeFile(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), "text.txt")
#############################################################
consumeFile <- function(api_key, requestURL, infileName, globalParam = setNames(list(), character(0)), outfileName = "results.csv", batchSize = 250, retryDelay = 0.3) {
  if (missing(api_key)) {
    stop("Need to specify API key")
  }
  if (missing(infileName)) {
    stop("Need to specify file to be scored")
  }
  if (missing(requestURL)) {
    stop("Need to specify request URL")
  }
  #read file into dataframe, convert into dataframe
  valuesDF = read.csv(infileName,check.names=FALSE)
  df <- data.frame(stringsAsFactors=FALSE)
  valuebatch = data.frame(stringsAsFactors=FALSE)
  counter = 1
  lastproc = 0

  #process in batches and make API calls in batches
  for(i in 1:(nrow(valuesDF))) {
    if(counter == batchSize || i == (nrow(valuesDF))) {
      resultDF = data.frame(stringsAsFactors=FALSE)
      valuebatch = valuesDF[(lastproc+1):i,]
      keyvalues = rjson::fromJSON((df2json::df2json(valuebatch)))
      temp <- callAPI(api_key, requestURL, keyvalues, globalParam, retryDelay)
      lastproc = i
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1
      resultDF <- data.frame(resultList[,(ncol(resultList))])
      if(length(df) != 0 && length(resultDF) != 0) {
        names(df) <- names(resultDF)
      }
      df <- rbind(df,resultDF)

      print(sprintf("%i %s %i %s", i,"out of",nrow(valuesDF),"processed"))
      valuebatch = data.frame(stringsAsFactors=FALSE)
      counter = 0
    }
    counter = counter + 1
  }
  colnames(df) <- "Scored probabilities"

  fileConn <-file(outfileName,"w")
  write.csv(df, fileConn)
  close(fileConn)
  return (df)
}



#############################################################
#' @title Consume Data Table
#' @description
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
#' @examples
#' # Consuming a newly published titanic demo webservice
#' # First, consume with inputs as a list
#' response <- consumeDataTable(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))
#############################################################
consumeDataTable <- function(api_key, requestURL, columnNames, ..., globalParam="", retryDelay = 0.3) {
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
  results <- callDTAPI(api_key, requestURL, columnNames, valuesList,  globalParam, retryDelay)
  results <- jsonlite::fromJSON(results)

  resultValues = results$Results$output1$value
  # Previous lines were commented out, would not return correctly if there were multiple return values
  #resultDF <- data.frame(resultList[,(ncol(resultList))])
  #colnames(resultDF) = "Scored probabilities"
  resultDF <- data.frame(resultValues$Values)
  colnames(resultDF) <- resultValues$ColumnNames
  return(resultDF)
}



#############################################################
#' @title Consume Lists
#' @description
#' This function takes in an API key, the request URL (OData Endpoint Address), the column names and multiple requests
#' It scores the experiment with the requests stored in a list of lists, and sends it to the server in the appropriate format.
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the user. It returns the output column(s) along with the scored probablities!
#' @param string The api key must be entered as the first parameter
#' @param string requestURL must be entered as the third parameter
#' @param list columnNames - column Names
#' @param ... each parameter must be a request in the format of a list that contains a row of values corresponding to the column names provided
#' @param string globalParam - global parameters, default value is ""
#' @param int retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list
#' @examples
#' # First, consume as lists
#' # First, consume with inputs as a list
#' response <- consumeLists(endpoints[[1]]["PrimaryKey"], paste(endpoints[[1]]["ApiLocation"], "/execute?api-version=2.0&details=true",sep=""), list("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare"), list(1, "male", 20, 2, 0, 8.50), list(1, "female", 20, 1, 0, 8.50))
#############################################################
consumeLists <- function(api_key, requestURL, ..., globalParam = setNames(list(), character(0)), retryDelay = 0.3) {
  if (missing(api_key)) {
    stop("Need to specify API key")
  }

  if (missing(requestURL)) {
    stop("Need to specify request URL")
  }
  if(missing(globalParam)) {
    globalParam = setNames(list(), character(0))
  }
  df <- data.frame(stringsAsFactors=FALSE)
  #store arguments as mega list of lists
  keyvalues <- list(...)
  #make api call with components of payload
  temp <- callAPI(api_key, requestURL, keyvalues,  globalParam, retryDelay)
  resultStored <- jsonlite::fromJSON(temp)
  resultList = resultStored$Results$output1
  resultDF <- data.frame(resultList[,(ncol(resultList))])

  df <- rbind(df,resultDF)
  colnames(df) <- "Scored probabilities"
  return(df)
}



#############################################################
#' @title Consume Data Frame
#' @description
#' This function takes in an API key, the request URL (OData Endpoint Address), the column names and multiple requests
#' It scores the experiment with the requests stored in a list of lists, and sends it to the server in the appropriate format.
#' It then obtains a response from Azure Machine Learning Studio and returns a response to the user. It returns the output column(s) along with the scored probablities!
#' @param string The api key must be entered as the first parameter
#' @param string requestURL must be entered as the second parameter
#' @param string valuesDF - The name of the data frame that is being scored
#' @param string globalParam - global parameters, default value is ""
#' @param int batchSize of each batch, which is optional, but 100 by default
#' @param int retryDelay the time in seconds to delay before retrying in case of a server error, default value of 0.3 seconds
#' @return results in a list of lists, with the scored probability at the end of each list
#' @examples
#' # Using the titanic as the example code and model
#' # consume with inputs as dataframe
#' # creating test data.frame
#' demoDF <- data.frame("Pclass"=c(1,2,1), "Sex"=c("male","female","male"), "Age"=c(8,20, 30), "Parch"=c(1,1,1), "SibSp"=c(1,3,1), "Fare"=c(10,7.5, 9))
#' responseDF <- consumeDataframe(TitanicService[[2]][[1]]$PrimaryKey, paste(TitanicService[[2]][[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), demoDF)
#############################################################
consumeDataframe <- function(api_key, requestURL, valuesDF, globalParam=setNames(list(), character(0)), batchSize = 250, retryDelay = 0.3) {
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
  df <- data.frame(stringsAsFactors=FALSE)
  valuebatch = list()
  counter = 1

  #process in batches and make API calls in batches
  for(i in 1:(length(values))) {
    valuebatch[length(valuebatch) + 1] = values[i]
    if(counter == batchSize || i == (length(values))) {
      temp <- callDTAPI(api_key, requestURL, columnNames, valuebatch, globalParam, retryDelay)
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1$value$Values
      resultDF <- data.frame(resultList[,(ncol(resultList))])
      #      print(resultDF)
      #      print(is.data.frame(resultDF))
      if(length(df) != 0 && length(resultDF) != 0) {
        names(df) <- names(resultDF)
      }
      df <- rbind(df,resultDF)
      colnames(df) <- "Scored probabilities"

      #      print("passed")
      print(sprintf("%i out of %i processed", i, length(values)))
      valuebatch = list()
      counter = 0
    }
    counter = counter + 1
  }
  colnames(df) <- "Scored probabilities"
  return(df)
  #   resultStored <- jsonlite::fromJSON(resultStored)
  #   resultDF <- data.frame(matrix(resultStored$Results$output1$value$Values))
  #   colnames(resultDF) <- resultStored$Results$output1$value$ColumnNames
  #   return(resultDF)
}



#############################################################
#' @title HELPER FUNCTION: Call DT API
#' @description
#' This function is a helper that takes in an API key, request URL, column names of the data, request in the data table format (in a lists of lists), global parameters of a web service, and delay time before retrying a call in case of a server error.
#' It then obtains a response from Azure Machine Learning Studio in the JSON format and returns a response to the consumption functions that call it.
#' @param string apiKey
#' @param string requestUrl entered as a string or discovered through the discover schema method
#' @param list columnNames - column names entered as a list or discovered through the discover schema method
#' @param list requestList
#' @param list globalParam - global parameters entered as a list, default value is an empty list
#' @param int retryDelay the time in seconds to delay before retrying in case of a server error
#' @return result from the DT API Call
#############################################################
callDTAPI <- function(api_key, requestURL, columnNames, values,  globalParam, retryDelay) {
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
      formatresult = result
      #      formatresult <- jsonlite::toJSON(jsonlite::fromJSON(result), pretty = TRUE)

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
      print(h$value())
      break
    }
  }
  return(formatresult)
}



#############################################################
#' @title HELPER FUNCTION: Call API
#' @export internal
#' @description
#' This function is a helper that takes in an API key, request URL, request in the key value format (in a lists of lists), global parameters of a web service, and delay time before retrying a call in case of a server error.
#' It then obtains a response from Azure Machine Learning Studio in the JSON format and returns a response to the consumption functions that call it
#' @param string apiKey
#' @param string requestUrl entered as a string or discovered through the discover schema method
#' @param list columnNames - column names entered as a list or discovered through the discover schema method
#' @param requestList
#' @param list globalParam - global parameters entered as a list, default value is an empty list
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return result of the API call
#############################################################
callAPI <- function(api_key, requestURL, keyvalues,  globalParam, retryDelay) {
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
          input1 = keyvalues
        )
        ,GlobalParameters = globalParam
      )
      body = enc2utf8((rjson::toJSON(req)))
      print(body)

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
      formatresult = result

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



#############################################################
#' @title HELPER FUNCTION: Discover Schema
#' @export internal
#' @description
#' This function returns to the user a list of optional functions the user can perform and the returns the schema of their requested workspace.
#' @param string wkID - workspace ID retrieved from your AzureML account
#' @param string token - the authentication token retrieved from your AzureML account
#' @param string schemes - default is https
#' @param string host - default is "requestresponse001.cloudapp.net:443"
#' @param string api_version - default api version is 2.0
#' @return The schema of the call the user made
#############################################################
discoverSchema <- function(wkID, token, schemes = "https", host = "requestresponse001.cloudapp.net:443", api_version = "2.0") {
  # swagger document:
  # schemes://hostbasepath/"swagger.json?api-version=2.0"
  swaggerURL = paste(schemes,"://", host, "/workspaces/", wkID, "/services/", token,"/swagger.json?api-version=",api_version, sep = "")


  httr::set_config(config(ssl_VERIFYHOST=FALSE,ssl_verifyPEER=FALSE), override=TRUE)
  resp <- httr::GET(swaggerURL)

  swagger <- httr::content(resp)

  # condensed three steps into one line: Access JSON and then use rjson and json lite in order to structure it as a layered json object
  inputschema = jsonlite::toJSON(jsonlite::fromJSON((rjson::toJSON(swagger$definitions$ExecutionInputs))), pretty = TRUE)
  inputexample <- jsonlite::toJSON(jsonlite::fromJSON((rjson::toJSON(swagger$definitions$ExecutionRequest$example))), pretty = TRUE)

  #find the path where operationId is execute
  foundExec = FALSE
  pathno = 0
  foundpathindex= -1
  for(execpath in swagger$paths) {
    pathno = pathno + 1
    for(operationpath in execpath) {
      for(operation in operationpath) {
        for(charac in operation) {
          if(charac[1] == "execute")
          {
            foundExec = TRUE
            foundpathindex = pathno
            break
          }
        }
      }
    }
  }

  executepath = names(swagger$paths)[[foundpathindex]]
  httpMethod = toupper(names(swagger$paths[[2]]))
  # requestURL:
  #   "https://requestresponse001.cloudapp.net:443/workspaces/7e8f135f31274b7eac419bd056875c03/services/a5b003e52c924d16a2e38ade45dd0154/execute?api-version=2.0&format=swagger"
  #   schemes://hostbasepath(path where operationId="execute")
  requestURL = paste(schemes,"://", host, "/workspaces/", wkID, "/services/", token, executepath, sep = "")

  httpRequest = paste(httpMethod,requestURL)
  #tell user what they can do
  if(foundExec) {
    consumefile = paste("consumeFile(api_key, requestURL, dataframe)")
    consumedf = paste("consumeDataframe(api_key, requestURL, valuesDF)")
    consumelists = paste("consumeLists(api_key, requestURL, ...)")
    consumedt = paste("consumeFile(api_key, requestURL, columnNames, ...)")

    cat("Sample functions to execute the web service and get a response synchronously:","\n", consumefile,"\n", consumedf,"\n", consumelists,"\n", consumedt,"\n","\n")

  }

  return (list("Request URL:" = requestURL, "Sample input:" = inputexample, "Input schema:" = inputschema))
}
