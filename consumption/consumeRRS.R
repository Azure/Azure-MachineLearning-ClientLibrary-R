library("RCurl")
library("rjson")
library("data.table")
library("df2json")
library("jsonlite")
library("httr")


################################################################################################################################
#' This function discovers using the workspace ID and web service ID, information specific to the consumption functions
#' @param workspaceId workspace ID
#' @param helpURL help page URL
#' @param scheme the URI scheme
#' @param host optional parameter that defaults to ussouthcentral.services.azureml.net
#' @param api_version that defaults to 2.0
#' @return List containing the request URL of the webservice, column names of the data, sample input as well as the input schema
################################################################################################################################

discoverSchema <- function(workspaceId, helpURL, scheme = "https", host = "requestresponse001.cloudapp.net:443", api_version = "2.0") {
  endpointID = getEndpointFromUrl(helpURL)
  # Construct swagger document URL using parameters
  # Use paste method without separator
  swaggerURL = paste(scheme,"://", host, "/workspaces/", workspaceId, "/services/", endpointID,"/swagger.json?api-version=",api_version, sep = "")
  # Uses httr package to get the HTTP response from the swagger document URL
  httr::set_config(config(ssl_VERIFYHOST=FALSE,ssl_verifyPEER=FALSE), override=TRUE)
  response <- httr::GET(swaggerURL)
  # Automatically parses the content and gets the swagger document
  swagger <- httr::content(response)

  # Accesses the input schema in the swagger document
  inputSchema = swagger$definition$input1Item
  
  #Accesses the example in the swagger document and converts it to JSON 
  exampleJson <- rjson::toJSON(swagger$definitions$ExecutionRequest$example)
  
  #Accesses a single specific JSON object and formats it to be a request inputted as a list in R
  inputExample = as.list((jsonlite::fromJSON((exampleJson)))$Inputs$input1)
  
  for(i in 1:length(inputExample)) {
    if(typeof(inputExample[[i]]) == "character") {
      inputExample[i] = "Please input valid String"
    }
  }
  #Accesses the names of the columns in the example and stores it in a list of column names
  columnNames = list()
  for(i in 1:length(inputExample)) {
    columnNames[[i]] = names(inputExample)[[i]]
  }
  # Uses multiple nested loops to access the various paths in the swagger document and find the execution path
  foundExecPath = FALSE
  pathNo = 0
  execPathNo= -1
  for(execPath in swagger$paths) {
    pathNo = pathNo + 1
    for(operationpath in execPath) {
      for(operation in operationpath) {
        #Goes through the characteristcs in every operation e.g. operationId
        for(charac in operation) {
          # Finds the path in which the operationId (characteristic of the path) = execute and sets the execution path number
          if(charac[1] == "execute")
          {
            #Sets found execution path to true
            foundExecPath = TRUE
            execPathNo = pathNo
            break
          }
        }
      }
    }
  }

  #Stores the execution path
  if(foundExecPath) {
    executePath = names(swagger$paths)[[execPathNo]]
  } else{
    executePath = "Path not found"
  }
  # Constructs the request URL with the parameters as well as execution path found. The separator is set to an empty string 
  requestUrl = paste(scheme,"://", host, "/workspaces/", workspaceId, "/services/", endpointID, executePath, sep = "")

  # Access the HTTP method type e.g. GET/ POST and constructs an example request
  httpMethod = toupper(names(swagger$paths[[2]]))
  httpRequest = paste(httpMethod,requestUrl)
  # Tell user what functions they can use and prints to the console
  if(foundExecPath) {
    consumeFile = paste("To score a file: consumeFile(apiKey, requestUrl, dataframe)")
    consumeDataFrame = paste("To score a dataframe: consumeDataframe(apiKey, requestUrl, scoreDataFrame)")
    consumeLists = paste("To score requests as lists in the key-value format: consumeLists(apiKey, requestUrl, ...)")
    cat("Sample functions to execute the web service and get a response synchronously:","\n", consumeFile,"\n", consumeDataFrame,"\n", consumeLists,"\n","\n")
    
  } else {
    cat("Warning! There was no execution path found for this web service, hence a request URL cannot be constructed!","\n","\n")
  }
  # Warns user of characters and urges them to enter valid strings for them
  firstWarning = TRUE
  for(i in 1:length(inputExample)) {
    if(typeof(inputExample[[i]]) == "character") {
      if(firstWarning) {
        cat("Warning! The sample input does not contain sample values for characters. Please input valid Strings for these fields", "\n")
      }
      cat("   ", names(inputExample)[[i]],"\n")
      firstWarning = FALSE
    }
  }
  
  #Returns what was discovered in the form of a list
  return (list("requestUrl" = requestUrl, "columnNames" = columnNames, "sampleInput" = inputExample, "inputSchema" = inputSchema))
}

################################################################################################################################
#' This function takes in an API key, file name and the request URL as compulsory parameters.
#' It scores a file and returns results in a data frame.
#' It calls a helper function that sends requests to the server in the key-value format.
#' It processes requests in batches and stores the responses in order of batches in an array. It returns the results in a data frame, and stores the results in a text file.
#' @param apiKey entered as a string
#' @param requestUrl entered as a string or discovered through the discover schema method
#' @param inFileName the name of the file scored as a string
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param outputFileName the name of the file to output results to, entered as a string, default value is "results.csv"
#' @param batchSize batch size of each batch, default value is 250
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
################################################################################################################################

consumeFile <- function(apiKey, requestUrl, inFileName, globalParam = setNames(list(), character(0)), outputFileName = "results.csv", batchSize = 300, retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields
  if (missing(apiKey)) {
    stop("Need to specify API key")
  }
  if (missing(inFileName)) {
    stop("Need to specify file to be scored")
  }
  if (missing(requestUrl)) {
    stop("Need to specify request URL")
  }
  #read file and store as a data frame to be scored
  scoreDataFrame = read.csv(inFileName,check.names=FALSE)
  # create empty data frame that stores results to be returned
  returnDataFrame <- data.frame(stringsAsFactors=FALSE)
  # create data frame that stores requests in each batch
  requestBatch = data.frame(stringsAsFactors=FALSE)
  counter = 1
  lastProc = 0
  
  # Loop that iterates through the rows of the entire data frame that is to be scored
  for(i in 1:(nrow(scoreDataFrame))) {
    # If we have reached the batch size provided or the end of the data frame
    if(counter == batchSize || i == (nrow(scoreDataFrame))) {
      # Create empty data frame that stores results for that batch
      batchResults = data.frame(stringsAsFactors=FALSE)
      # Store a single batch of requests in a data frame
      requestBatch = scoreDataFrame[(lastProc+1):i,]
      # Convert them into key-value lists using rjson and df2json packages
      keyvalues = rjson::fromJSON((df2json::df2json(requestBatch)))
      # Store results returned from call in temp variable
      temp <- callAPI(apiKey, requestUrl, keyvalues, globalParam, retryDelay)
      # Set last processed to current row
      lastProc = i
      # Access output by converting from JSON into list and indexing into Results
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1
      batchResults <- data.frame(resultList)
      # Force returnDataFrame to have the same column names to avoid errors
      if(length(returnDataFrame) != 0 && length(batchResults) != 0) {
        names(returnDataFrame) <- names(resultList)
      }
      #Add batch results to the dataframe to be returned
      returnDataFrame <- rbind(returnDataFrame,batchResults)
      #Print how many rows in total have been processed
      print(sprintf("%i %s %i %s", i,"out of",nrow(scoreDataFrame),"processed"))
      #Reset the requests in the batch to empty data frame
      requestBatch = data.frame(stringsAsFactors=FALSE)
      counter = 0
    }
    counter = counter + 1
  }

  # Write results to a csv file
  resultsFile <-file(outputFileName,"w")
  write.csv(returnDataFrame, resultsFile)
  close(resultsFile)
  return (returnDataFrame)
}


################################################################################################################################
#' This function takes in an API key, request URL, requests as lists in the key value format as compulsory parameters.
#' It scores requests with lists entered in the key-value format and returns results in a data frame.
#' It calls a helper function that sends requests to the server in the key value format.
#' It processes requests in batches and stores the responses in order of batches in an array. It returns the results in a data frame.
#' @param apiKey entered as a string
#' @param requestUrlDataTable request URL entered as a string or discovered through the discover schema method
#' @param ... variable number of requests entered as lists in the key-value format
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
################################################################################################################################
consumeLists <- function(apiKey, requestUrlDataTable, ..., globalParam = setNames(list(), character(0)), retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields

  if (missing(apiKey)) {
    stop("Need to specify API key")
  }
  
  if (missing(requestUrlDataTable)) {
    stop("Need to specify request URL")
  }
  if(missing(globalParam)) {
    globalParam = setNames(list(), character(0))
  }
  
  # Store variable number of lists entered as a list of lists
  requestsLists <- list(...)
  # Make API call with parameters
  result <- callAPI(apiKey, requestUrlDataTable, requestsLists,  globalParam, retryDelay)
  # Access output by converting from JSON into list and indexing into Results 
  resultStored <- jsonlite::fromJSON(result)
  resultList = resultStored$Results
  # Store results in a data frame
  resultDataFrame = data.frame(stringsAsFactors=FALSE)
  for(output in resultList) {
    resultDataFrame <- rbind(resultDataFrame,data.frame(output))
    colnames(resultDataFrame) = colnames(output)
  }
  return(resultDataFrame)
}

################################################################################################################################
#' This function takes in an API key, data frame and the request URL as compulsory parameters.
#' It scores a data frame and returns results in a data frame.
#' It calls a helper function that sends requests to the server in the key-value format.
#' It processes requests in batches and stores the responses in order of batches in an array.
#' @param apiKey entered as a string
#' @param requestUrl entered as a string or discovered through the discover schema method
#' @param scoreDataFrame the data frame to be scored
#' @param globalParam global parameters entered as a list, default value is an empty list
#' @param batchSize batch size of each batch, default value is 250
#' @param retryDelay the time in seconds to delay before retrying in case of a server error, default value is 0.3 seconds
#' @return returnDataFrame data frame containing results returned from web service call
################################################################################################################################

consumeDataframe <- function(apiKey, requestUrl, scoreDataFrame, globalParam=setNames(list(), character(0)), batchSize = 300, retryDelay = 0.3) {
  #Stops users if they miss out mandatory fields
  
  if (missing(apiKey)) {
    stop("Need to specify API key")
  }
  
  if (missing(requestUrl)) {
    stop("Need to specify request URL")
  }
  if (missing(scoreDataFrame)) {
    stop("Need to specify dataframe to be scored")
  }

  # create empty data frame that stores results to be returned
  returnDataFrame <- data.frame(stringsAsFactors=FALSE)
  # create data frame that stores requests in each batch
  requestBatch = data.frame(stringsAsFactors=FALSE)
  counter = 1
  lastProc = 0
  
  # Loop that iterates through the rows of the entire data frame that is to be scored
  for(i in 1:(nrow(scoreDataFrame))) {
    # If we have reached the batch size provided or the end of the data frame
    if(counter == batchSize || i == (nrow(scoreDataFrame))) {
      # Create empty data frame that stores results for that batch
      batchResults = data.frame(stringsAsFactors=FALSE)
      # Store a single batch of requests in a data frame
      requestBatch = scoreDataFrame[(lastProc+1):i,]
      # Convert them into key-value lists using rjson and df2json packages
      keyvalues = rjson::fromJSON((df2json::df2json(requestBatch)))
      # Store results returned from call in temp variable
      temp <- callAPI(apiKey, requestUrl, keyvalues, globalParam, retryDelay)
      # Set last processed to current row
      lastProc = i
      # Access output by converting from JSON into list and indexing into Results
      resultStored <- jsonlite::fromJSON(temp)
      resultList = resultStored$Results$output1
      batchResults <- data.frame(resultList)
      # Force returnDataFrame to have the same column names to avoid errors
      if(length(returnDataFrame) != 0 && length(batchResults) != 0) {
        names(returnDataFrame) <- names(resultList)
      }
      #Add batch results to the dataframe to be returned
      returnDataFrame <- rbind(returnDataFrame,batchResults)
      #Print how many rows in total have been processed
      print(sprintf("%i %s %i %s", i,"out of",nrow(scoreDataFrame),"processed"))
      #Reset the requests in the batch to empty data frame
      requestBatch = data.frame(stringsAsFactors=FALSE)
      counter = 0
    }
    counter = counter + 1
  }
  return(returnDataFrame)
}


########################################################### HELPER FUNCTION ###########################################################
#' This function is a helper that takes in an API key, request URL, request in the key value format (in a lists of lists), global parameters of a web service, and delay time before retrying a call in case of a server error.
#' It then obtains a response from Azure Machine Learning Studio in the JSON format and returns a response to the consumption functions that call it.
#######################################################################################################################################

callAPI <- function(apiKey, requestUrl, keyvalues,  globalParam, retryDelay) {
  # Set number of tries and HTTP status to 0
  httpStatus = 0
  tries = 0
  # Limit number of API calls to 3
  for(i in 1:3) {
    # In case of server error or if first try,
    if(tries == 0 || httpStatus >= 500) {
      if(httpStatus >= 500) {
        # Print headers and let user know you are retrying
        print(paste("The request failed with status code:", httpStatus, sep=" "))
        print("headers:")
        print(headers)
        print(sprintf("%s %f %s", "Retrying in ",retryDelay," seconds"))
        # Delay by specified time in case of server error
        Sys.sleep(retryDelay)
      }
      tries = tries + 1
      # Load RCurl package functions
      options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
      h = RCurl::basicTextGatherer()
      hdr = RCurl::basicHeaderGatherer()
      # Construct request payload
      req = list(
        Inputs = list(
          input1 = keyvalues
        )
        ,GlobalParameters = globalParam
      )
      # Convert request payload to JSON
      body = enc2utf8((rjson::toJSON(req)))
      # Create authorization header
      authz_hdr = paste('Bearer', apiKey, sep=' ')
      
      # Make call to API with necessary components
      h$reset()
      RCurl::curlPerform(url = requestUrl,
                         httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                         postfields=body,
                         writefunction = h$update,
                         headerfunction = hdr$update,
                         verbose = TRUE           
                         # Parameters below are needed if using test environment, but should not be included for security reasons                  
                         ,ssl.verifypeer=FALSE,
                         ssl.verifyhost = FALSE
      )
      # Gather headers
      headers = hdr$value()
      # Get HTTP status to decide whether to throw bad request or retry, or return etc.
      httpStatus = headers["status"]
      result = h$value()
    }
    # Return result if successful
    if(httpStatus == 200) {
      return(result)
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
  return(result)
}

########################################################### HELPER FUNCTION ###########################################################
#' This function is a helper that takes in the help URL, and parses the endpoint from it
#' This function also documents the assumption that the help URL will end in the format "endpoints/"endpointID/(other keywords)
#######################################################################################################################################

getEndpointFromUrl <- function(helpURL) {
  return (strsplit(((strsplit(helpURL,"endpoints/"))[[1]][2]),"/")[[1]][[1]])
}