#' Discover, publish and consume Microsoft Azure Web Services
#'
#' @description
#'
#' This package implements an interface with Microsoft Azure Machine Learning, allowing you to publish a function, e.g. a prediction function using a trained machine learning model, as a web service from which it can be utilized by users on Visual Studio, Excel, etc., or consumed within R itself.
#'
#' You are also able to access pre-existing web service endpoints to consume directly from R.
#'
#' @section Finding the workspace ID and authorization token:
#'
#' All functions require a workspace ID (\code{wsID}) and authorization token (\code{authToken}). You can find these on the settings tab of https://studio.azureml.net, but users can make a temporary free account at \url{https://azure.microsoft.com/en-us/pricing/free-trial/}.
#'
#' For more information, see: \url{https://github.com/Azure/Azure-MachineLearning-ClientLibrary-R} or refer to the vignette included in this package.
#'
#' @section Summary of functions:
#'
#' 1. Discovery
#'
#' \itemize{
#'    \item Get web services: \code{\link{getWebServices}}
#'    \item Get web service details \code{\link{getWSDetails}}
#'    \item Get endpoints: \code{\link{getEndpoints}}
#'    \item Get endpoint details: \code{\link{getEPDetails}}
#' }
#'
#' 2. Publish
#'
#' \itemize{
#'    \item Publish a new web service: \code{\link{publishWebService}}
#'    \item Update an existing web service: \code{\link{updateWebService}}
#' }
#'
#' 3. Consume
#'
#' \itemize{
#'    \item Discover web service schema: \code{\link{discoverSchema}}
#'    \item Score a file: \code{\link{consumeFile}}
#'    \item Score a dataframe: \code{\link{consumeDataframe}}
#'    \item Score data as lists: \code{\link{consumeLists}}
#' }
#'
#' @name AzureML-package
#' @aliases AzureML
#' @docType package
#' @keywords package
NULL
