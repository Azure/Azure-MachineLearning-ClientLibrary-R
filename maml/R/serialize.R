library(rjson)

################################################################
# Serialization/DeSerialization
# jsonlite needs to be installed:
#     install.packages(jsonlite)
################################################################

#Define global variables for serialization
inputDF <<- data.frame()
outputDF <<- data.frame()
publishedFunction <<- getFunctionString



#############################################################
#' @title Serialize the input
#' @description Create a serialized dataFrame with the input arguments
#'
#' Expecting:
#'    inputSchema = list("arg1"="type", "arg2"="type", ...)
#' @param list Take arguments of a function in type list
#' @return serialized input expectations
#############################################################
serializeI <- function(input) {
  #convert input to vectors to be added to dataframe
  inArgs <<- unlist(input)
  inputDF <<- data.frame(inArgs)

  #serialize data
  sArgs = jsonlite::serializeJSON(inArgs)

  return(sArgs)
}



#############################################################
#' @title Serialize the output
#' @description Serialize the output DataFrame
#' @param list Take arguments of a function in type list
#' Expecting:
#'    outputSchema = list("output1"="type", "output2"="type", ...)
#' @return serialized output expectations
#############################################################
serializeO <- function(output) {
  #convert output to vectors to be added to dataframe
  outArgs <<- unlist(output)
  outputDF <<- data.frame(outArgs)

  #serialize data
  oArgs <- jsonlite::serializeJSON(outArgs)
  return (oArgs)
}



#############################################################
#' @title Serialize the Body of a Function
#' @description Change function to accept a dataframe
#' @param string function name to serialize
#' @return output (fromJSON)
#############################################################
serializeFunc <- function(publishedFunction) {
  # serialize body of function
  bodyFunction <- body(publishedFunction)

  return (jsonlite::serializeJSON(bodyFunction))
}



#############################################################
#' @title Serialize (Published) User Arguments
#' @description Take Output from function and save into output dataframe
#' Expect arguments from user in a dataframe
#' @param dataframe Pass dataframe into function
#' @return serialized output
#############################################################
serializeMeta <- function(userDF, funcName) {
  # convert back to params
  args <- as.list(userDF[,])

  #run function
  output <- do.call(funcName, args)
  print(output)
  returnDF <- data.frame(output)

  #serialize output
  oArgs = jsonlite::serializeJSON(returnDF)
  return (oArgs)
}



#############################################################
#' @title Serialize (Published) Output
#' @description This is an internal function
#' Take Output from function and save into output dataframe
#' @param dataframe Pass dataframe back to user
#' @return output dataframe
#############################################################
unserializeMeta <- function(output) {
  # convert back to params
  outputDF <<- jsonlite::unserializeJSON(output)
  outlist <- as.list(outputDF[,])

  print(outlist)
  return (outlist)
}
