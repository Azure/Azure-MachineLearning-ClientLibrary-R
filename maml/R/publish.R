#' @docType package
#' @name publish
#' Publishing is a simple one call function for the user. Upon calling, the user simply needs to provide
#' the workspace identification information, the function they want published and the name they want this
#' service to be saved as. The publish function call will handle the API call for the user and any consumption
#' that may need to be done related to this function. Once the call is finished, the function will return a
#' list with the web service details, the endpoint detains and the consumption function to the user.
#' There is also an update publish call so that the user can republish a function without creating a new instance.



#############################################################
# String constants
#############################################################
publishURL <- "https://management.azureml-int.net/workspaces/%s/webservices/%s" ## REMOVE SSL IGNORING FOR REAL VERSION ##
#publishURL <- "https://hiteshsm.cloudapp.net/workspaces/%s/webservices/%s" ## REMOVE SSL IGNORING FOR REAL VERSION ##
wrapper <- "inputDF <- maml.mapInputPort(1)\r\noutputDF <- matrix(ncol = %s, nrow = nrow(inputDF))\r\ncolnames(outputDF) <- list(%s)\r\noutputDF <- data.frame(outputDF)\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(paste(\"src\", file, sep=\"/\"), lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(strsplit(file, \"\\\\.\")[[1]][[1]], character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\nfor (i in 1:nrow(inputDF)) {\r\n  outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n}\r\nmaml.mapOutputPort(\"outputDF\")"



#############################################################
#' @title Get function source code as a string
#' @description
#' This is a helper function that will convert a function's source code to a string
#' @export internal
#' @param x Name of the function to convert to a string
#' @return function in string format
#############################################################
getFunctionString <- function (x)
{
  if (tryCatch(!is.character(x), error = function(e) TRUE))
    x <- as.character(substitute(x))
  objs <- list()
  where <- character()
  visible <- logical()
  if (length(pos <- find(x, numeric = TRUE))) {
    objs <- lapply(pos, function(pos, x) get(x, pos = pos),
                   x = x)
    where <- names(pos)
    visible <- rep.int(TRUE, length(pos))
  }
  if (length(grep(".", x, fixed = TRUE))) {
    np <- length(parts <- strsplit(x, ".", fixed = TRUE)[[1L]])
    for (i in 2:np) {
      gen <- paste(parts[1L:(i - 1)], collapse = ".")
      cl <- paste(parts[i:np], collapse = ".")
      if (gen == "" || cl == "")
        next
      Call <- substitute(getS3method(gen, cl, TRUE), list(gen = gen,
                                                          cl = cl))
      f <- eval.parent(Call)
      if (!is.null(f) && !is.null(environment(f))) {
        ev <- topenv(environment(f), baseenv())
        nmev <- if (isNamespace(ev))
          getNamespaceName(ev)
        else NULL
        objs <- c(objs, f)
        msg <- paste("registered S3 method for", gen)
        if (!is.null(nmev))
          msg <- paste(msg, "from namespace", nmev)
        where <- c(where, msg)
        visible <- c(visible, FALSE)
      }
    }
  }
  for (i in loadedNamespaces()) {
    ns <- asNamespace(i)
    if (exists(x, envir = ns, inherits = FALSE)) {
      f <- get(x, envir = ns, inherits = FALSE)
      objs <- c(objs, f)
      where <- c(where, paste("namespace", i, sep = ":"))
      visible <- c(visible, FALSE)
    }
  }
  ln <- length(objs)
  dups <- rep.int(FALSE, ln)
  if (ln > 1L)
    for (i in 2L:ln) for (j in 1L:(i - 1L)) if (identical(objs[[i]],
                                                          objs[[j]], ignore.environment = TRUE)) {
      dups[i] <- TRUE
      break
    }
  res <- list(name = x, objs = objs, where = where, visible = visible,
              dups = dups)
  class(res) <- "getAnywhere"

  #don't show the full response!
  #res
  # Might return multiple objects in a list, currently returning first object (BIG ASSUMPTION)
  #return(objs[1])
  return(gsub("\n", "\r\n", gsub("\"", "\\\"", objs[1])))
}



#############################################################
#' @title Package a function's dependencies into a base64 encoded string
#' @description
#' This is a helper function to extract object and package dependencies
#' then pack them into a .zip, then a base64 string
#' packDependencies()
#' @export internal
#' @param closure functionName - function to package dependencies from
#' @return encoded zip - will return false if nothing was zipped
#############################################################
packDependencies <- function(functionName) {

<<<<<<< HEAD
  # Recursive step for package packaging
  recurPkg <- function(pkgName, pkgList) {
    # if the package isn't already in the list
    if (!(pkgName %in% pkgList)) {
      # add it
      pkgList <- c(pkgName, pkgList)

      # if the package is available on a repo
      if (pkgName %in% row.names(available.packages())) {
        # iterate through the dependencies and check if need to add them
        for (pkg in strsplit(available.packages()[pkgName, "Depends"], split=", ")[[1]]) {
          # filter out duplicates, R version dependencies, and base packages
          if (!(pkg %in% pkgList) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
            # recursively call recurPkg
            pkgList <- recurPkg(pkg, pkgList)
=======
    # get in-memory objects
    else if (!is.function(name)) {
      dependencies[[obj]] <- name

      # Use the object's class to find package dependencies
      objClass <- class(name)

      # iterate through the class vector looking for packages
      for (class in objClass) {
        tryCatch({
          # get the name of the package the class belongs to
          nameEnv <- environment(get(class))
          # filter out basic objects
          if (!(identical(nameEnv, NULL)) && !(identical(nameEnv, .BaseNamespaceEnv))) {
            packages <- recurPkg(paste(getNamespaceName(nameEnv)), packages)
>>>>>>> a8474fd93f6dfb00d91b912c12d964345d297367
          }
        }
        # iterate through imports
        for (pkg in strsplit(available.packages()[pkgName, "Imports"], split=", ")[[1]]) {
          # filter out duplicates, R version dependencies, and base packages
          if (!(pkg %in% pkgList) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
            # recursively call recurPkg
            pkgList <- recurPkg(pkg, pkgList)
          }
        }
      }
    }
    # return updated list of packages
    return(pkgList)
  }

  # Recursive step for object packaging
  # NOTE: will not work if the user function specifies the names directly, e.g. won't find rjson::toJSON
  # from findGlobals man page: "R semantics only allow variables that might be local to be identified"
  recurDep <- function(objName, depList, pkgList) {
    # findGlobals() gets all external dependencies
    # Iterate over them
    for (obj in codetools::findGlobals(get(objName))) {
      name = get(obj)

      # filter out primitives and duplicates
      if (is.primitive(name) || (obj %in% names(depList))) {
        next
      }
      # non-function object dependencies
      else if (!is.function(name)) {
        depList[[obj]] <- name

        # Use the object's class to find package dependencies
        objClass <- class(name)

        # iterate through the class vector looking for packages
        for (class in objClass) {
          tryCatch({
            # get the name of the package the class belongs to
            nameEnv <- environment(get(class))
            # filter out basic environment
            if (!(identical(nameEnv, NULL)) && !(identical(nameEnv, .BaseNamespaceEnv))) {
              packages <- recurPkg(paste(getNamespaceName(nameEnv)), pkgList)
            }
          # if unable to find package, continue
          }, error = function(e) {
            sprintf("%s not found", obj)
          })
        }
      }
      # user defined functions
      else if (identical(environment(name), globalenv())) {
        depList[[obj]] <- name
        results <- recurDep(obj, depList, pkgList)
        depList <- results$dependencies
        pkgList <- results$packages
      }
      # functions from packages
      else if (paste(getNamespaceName(environment(name))) != "base") {
        packages <- recurPkg(paste(getNamespaceName(environment(name))), pkgList)
      }
    }
<<<<<<< HEAD
    return(list("dependencies"=depList, "packages"=pkgList))
=======
>>>>>>> a8474fd93f6dfb00d91b912c12d964345d297367
  }

  # call recurDep on the desired function and with empty lists
  results <- recurDep(functionName, list(), list())
  dependencies <- results$dependencies
  packages <- results$packages

  # save current path to restore later
  start = getwd()
  # go to package library, doing this to prevent zipping entire path to package
  toPack <- packages
  toZip = vector()
  for (i in 1:length(.libPaths())) {
    setwd(.libPaths()[i])
    # try to find the package in the directory and zip it
    for (pkg in toPack) {
      if (file.exists(pkg)) {
        # save it to original directory
        zip(paste(start, paste(pkg, "zip", sep="."), sep="/"), pkg)
        toZip <- c(toZip, paste(pkg, "zip", sep="."))
        # remove the package from the list of packages to pack
        toPack <- toPack[toPack != pkg]
      }
    }

    # if done packing, break
    if (length(toPack) == 0) {
      break
    }
  }

  # go back to where the user started
  setwd(start)

  # make sure that all packages were found
  if (length(toPack) > 0) {
    stop("Error: unable to locate one or more packages. Please make sure the packages used are in at least one of the library paths.")
  }

  # generate a GUID to act as a file name to store packages, R data
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))
  # dump objects, functions, etc. into .rdta file
  if (length(dependencies) > 0) {
    # maybe can save directly as a .zip and skip the zip() call?
    save(dependencies, file=guid)
    toZip <- c(toZip, guid)
  }

  # zip up everything
  if (length(toZip) > 0) {
    zip(zipfile=guid, files=toZip)
    zipEnc <- base64enc::base64encode(paste(guid, ".zip", sep=""))

    # delete the packages
    for (pkg in packages) {
      file.remove(paste(pkg, "zip", sep="."))
    }

<<<<<<< HEAD
    # delete the dependency rdta file
=======
>>>>>>> a8474fd93f6dfb00d91b912c12d964345d297367
    if (length(dependencies) > 0) {
      file.remove(guid)
      file.remove(paste(guid,"zip",sep="."))
    }

    # return the encoded zip as a string
    return(list(guid, zipEnc))
  }

  # if nothing was zipped, return empty string to indicate
  # returning two things because unable to return variable amounts
  else {
    return(list(guid, ""))
  }
}





#############################################################
#' @title HELPER FUNCTION: Convert Format
#' @description This is a helper function to convert expected schema to API-expecting format
#' @export internal
#' @param list argList - List of expected input parameters
#' @return Converted inputSchema to the proper format
#############################################################
convert <- function(argList) {
  form <- list()
  for (arg in names(argList)) {
    type = argList[[arg]]

    if (type == "float" || type == "double") {
      form[[ arg ]] <- list("type"="number", "format"=type)
    }
    else if (type == "date-time" || type == "string" || type == "time-span") {
      form[[arg]] <- list("type"="string", "format"=type)
    }
    else if (type == "uint16" || type == "int16" || type == "uint32" || type == "int32" || type == "uint64" || type == "int64") {
      form[[arg]] <- list("type"="integer", "format"=type)
    }
    else if (type == "int") {
      form[[arg]] <- list("type"="integer", "format"="int32")
    }
    else if (type == "bool" || type == "boolean") {
      form[[arg]] <- list("type"="boolean")
    }
    else {
      stop(sprintf("Error: data type \"%s\" not supported", type), call. = TRUE)
    }
  }
  return(form)
}



#############################################################
#' @title HELPER FUNCTION: Parameter Check
#' @description This is a helper function to check that the user has passed in all of the expected parameters.
#' @export internal
#' @param list userInput - List of expected input parameters
#' @param string funcName - The function that is being published
#' @return False if the input was not as expected/True if input matched expectation
#############################################################
paramCheck <- function(userInput, funcName) {
  numParamsEXPECTED <- length(formals(funcName))
  numParamsPASSED <- length(userInput)

  if (numParamsPASSED != numParamsEXPECTED) {
    errorWarning <- paste("Error: Your input Schema does not contain the proper input. You provided ", numParamsPASSED," inputs and ", numParamsEXPECTED," were expected",sep="")
    stop(errorWarning, call. = TRUE)
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}



#############################################################
#' @title Publish Web Service
#' @description
#' This function publishes code given a valid workspace ID and authentication token. The function expects the function name, service name, and
#' the input and output schemas from the user.
#' The user can expect a list of the web service details, the default endpoint details and the consumption function and use this information to access
#' the published function.
#' @param string functionName - The function that is being published
#' @param string serviceName - The name they would like the function published under
#' @param list inputSchema - List of expected input parameters
#' expecting inputSchema = list("arg1"="type", "arg2"="type", ...)
#' @param list outputSchema - List of expected output
#' expecting outputSchema = list("output1"="type", "output2"="type", ...)
#' @param string wkID - The workspace ID
#' @param string authToken - The primary authorization token
#' @return List of webservice details, default endpoint details, and the consumption function
#' @examples
#' # create a function to make predictions using the trained model
#' # For this example we will use the Titanic
#' I.e. predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare)
#' # Sample local call
#' predictTitanic(1, "male", 20, 2, 0, 8.50)
#' #Publish the function
#' TitanicService <- publishWebService("predictTitanic", "TitanicDemo", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)
#############################################################
publishWebService <- function(functionName, serviceName, inputSchema, outputSchema, wkID, authToken) {

  # Make sure schema inputted matches function signature
  paramCheck(inputSchema, functionName)

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Get and encode the dependencies
  zipString = packDependencies(functionName)

  # Build the body of the request, differing on whether or not there is a zip to upload
  if (zipString[[2]] == "") {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = convert(inputSchema),
        "OutputSchema" = convert(inputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName)))
      )
    )
  }
  else {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = convert(inputSchema),
        "OutputSchema" = convert(outputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName))),
        "ZipContents" = zipString[[2]]
      )
    )
  }

  # convert the payload to JSON as expected by API
  # TODO: consolidate json packages, i.e. use only one if possible
  body = RJSONIO::toJSON(req)
  #print(sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName))))

  # Response gatherer
  h = RCurl::basicTextGatherer()
  h$reset()

  # Generate unique guid to serve as the web service ID
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))

  # API call
  RCurl::httpPUT(url = sprintf(publishURL, wkID, guid), # defined above
                 httpheader=c('Authorization' = paste('Bearer', authToken, sep=' '),
                              'Content-Type' = 'application/json',
                              'Accept' = 'application/json'),
                 content = body,
                 writefunction = h$update)

  # Format output
  newService <- RJSONIO::fromJSON(h$value())

  # Use discovery functions to get endpoints for immediate use
  # NOTE: switch from internal URL for production
  endpoints <- getEndpoints(wkID, authToken, newService["Id"], internalURL)
  # add suffix to the API location so it can actually be used
  for (i in 1:length(endpoints)) {
    endpoints[[i]]$ApiLocation <- paste(endpoints[[i]]$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  }

  # currently returning list of webservice details (as a list) and endpoint details (as a list) in that order
  return(list("serviceDetails"=newService, "endpoints"=endpoints))
}



#############################################################
#' @title Update a Published Web Service
#' @description
#' This function updates published code given a valid workspace ID and authentication token. The function expects the function name, service id, and
#' the input and output schemas from the user.
#' The user can expect a list of the web service details, the default endpoint details and the consumption function and use this information to access
#' the published function.
#' @param string functionName - The function that is being updated
#' @param string serviceGUID - The name they would like the function published under
#' ((Note: cannot change the service name))
#' @param list inputSchema - List of expected input parameters
#' expecting inputSchema = list("arg1"="type", "arg2"="type", ...)
#' @param list outputSchema - List of expected output
#' expecting outputSchema = list("output1"="type", "output2"="type", ...)
#' @param string wkID - The workspace ID
#' @param string authToken - The primary authorization token
#' @return List of webservice details, default endpoint details, and the consumption function
#' @examples
#' # create a function to make predictions using the trained model
#' I.e. predictTitanic <- function (Pclass, Sex, Age, SibSp, Parch, Fare)
#' # Sample local call
#' predictTitanic(1, "male", 20, 2, 0, 8.50)
#' # Publish the function
#' TitanicService <- publishWebService("predictTitanic", "TitanicDemo", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)
#' # Let's say that predictTitanic was changed and we want to republish
#' TitanicService <- updateWebService("predictTitanic", "TitanicDemo", list("Pclass"="string", "Sex"="string", "Age"="int", "SibSp"="int", "Parch"="int", "Fare"="float"), list("survProb"="float"), wsID, wsAuth)
#############################################################
updateWebService <- function(functionName, wsID, inputSchema, outputSchema, wkID, authToken) {

  # Make sure schema inputted matches function signature
  paramCheck(inputSchema, functionName)

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Get and encode the dependencies
  zipString = packDependencies(functionName)

  # Build the body of the request, differing on whether or not there is a zip to upload
  if (zipString[[2]] == "") {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = convert(inputSchema),
        "OutputSchema" = convert(inputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName)))
      )
    )
  }
  else {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = convert(inputSchema),
        "OutputSchema" = convert(outputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), paste(sprintf("\"%s\"", names(outputSchema)), collapse=","), zipString[[1]], zipString[[1]], paste(getFunctionString(functionName))),
        "ZipContents" = zipString[[2]]
      )
    )
  }

  # convert the payload to JSON as expected by API
  # TODO: consolidate json packages, i.e. use only one if possible
  body = RJSONIO::toJSON(req)

  # Response gatherer
  h = RCurl::basicTextGatherer()
  h$reset()

  # API call
  RCurl::httpPUT(url = sprintf(publishURL, wkID, wsID),
                 httpheader=c('Authorization' = paste('Bearer', authToken, sep=' '),
                              'Content-Type' = 'application/json',
                              'Accept' = 'application/json'),
                 content = body,
                 writefunction = h$update)

  # Format output
  updatedService <- RJSONIO::fromJSON(h$value())

  # Use discovery functions to get default endpoint for immediate use
<<<<<<< HEAD
  # NOTE: switch from internal URL for production
  endpoints <- getEndpoints(wkID, authToken, newService["Id"], internalURL)
  for (i in 1:length(endpoints)) {
    endpoints[[i]]$ApiLocation <- paste(endpoints[[i]]$ApiLocation, "/execute?api-version=2.0&details=true",sep="")
  }

  # currently returning list of webservice details (as a list) and endpoint details (as a list) in that order
  return(list("serviceDetails"=updatedService, "endpoints"=endpoints))
=======
  # switch to getEndpoints() later
  defaultEP <- getEndpoints(wkID, authToken, newService["Id"], internalURL)

  # currently returning list of webservice details, default endpoint details, consumption function, in that order
  return(list(newService, defaultEP))#, consumption))
>>>>>>> a8474fd93f6dfb00d91b912c12d964345d297367
}
