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


#' \name{Serialization}
#' \title{Serialize Input}
#' \description{
#' serializeI stores the expected input parameters into a dataframe and then serializes the dataframe
#' }
#' \usage{serializeI(x)}
#' \arguments{
#' \item{x}{input schema
#' inputSchema = list("arg1"="type", "arg2"="type", ...)}
#' }
#' \value {The serialized input paramaters.}
#' 
#' Create a DataFrame with these arguments
#' ***Need to make dataframe global***
#' Expecting:
#'    inputSchema = list("arg1"="type", "arg2"="type", ...)
#' @param Take arguments of a function in type list
#' @return serialized input expectations
serializeI <- function(input) {
  #convert input to vectors to be added to dataframe
  inArgs <<- unlist(input)
  inputDF <<- data.frame(inArgs)
  
  #serialize data
  sArgs = jsonlite::serializeJSON(inArgs)
  
  return(sArgs)
}

#' \name{Serialization}
#' \title{Serialize Output}
#' \description{
#' serializeI stores the expected output parameters into a dataframe and then serializes the dataframe
#' }
#' \usage{serializeO(x)}
#' \arguments{
#' \item{x}{output schema
#' outputSchema = list("arg1"="type", "arg2"="type", ...)}
#' }
#' \value {The serialized output paramaters.}
#' 
#' Create a DataFrame with these arguments
#' ***Need to make dataframe global***
#' Expecting:
#'    outputSchema = list("output1"="type", "output2"="type", ...)
#' @param Take arguments of a function in type list
#' @return serialized output expectations
serializeO <- function(output) {
  #convert output to vectors to be added to dataframe
  outArgs <<- unlist(output)
  outputDF <<- data.frame(outArgs)
  
  #serialize data
  oArgs <- jsonlite::serializeJSON(outArgs)
  return (oArgs)
}

#' \name{Serialization}
#' \title{Serialize Function}
#' \description{
#' serializeI stores the expected output parameters into a dataframe and then serializes the dataframe
#' }
#' \usage{serializeO(x)}
#' \arguments{
#' \item{x}{output schema
#' outputSchema = list("arg1"="type", "arg2"="type", ...)}
#' }
#' \value {The serialized function body}
#' 
#' Serialized Body of Function
#' Change function to accept a dataframe
#' @param function
#' @return output (fromJSON)
serializeFunc <- function(publishedFunction) {
  # serialize body of function
  bodyFunction <- body(publishedFunction)
  
  return (jsonlite::serializeJSON(bodyFunction))
}
