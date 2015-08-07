#' Publish, discover, and consume Microsoft Azure Web Services
#'
#' This package implements an interface with Microsoft Azure Machine Learning, allowing users to publish a function, e.g. a prediction using a trained machine learning model, as a web service, to be shared with users on Visual Studio, Excel, etc., or consumed within R itself. Users are also available to access their preexisting web services and API endpoints available to their workspace to consume within R. All functions require a workspace ID (WsID) and authorization token (authToken) that can be found on the settings tab of studio.azureml.net, but users can make a temporary free account at \url{https://azure.microsoft.com/en-us/pricing/free-trial/}. For more information, check out this page: \url{https://github.com/Azure/Azure-MachineLearning-ClientLibrary-R}.
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
#'    \item Discover web service schema: \code{\link{publishWebService}}
#'    \item Score a file: \code{\link{consumeFile}}
#'    \item Score a dataframe: \code{\link{consumeDataframe}}
#'    \item Score data as lists: \code{\link{consumeLists}}
#' }
#'
#' @name maml-package
#' @aliases maml
#' @docType package
#' @keywords package
?NULL
