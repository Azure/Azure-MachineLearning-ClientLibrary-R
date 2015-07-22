#############################################################
# String constants
#############################################################
publishURL <- "https://management.azureml-int.net/workspaces/%s/webservices/%s" ## REMOVE SSL IGNORING FOR REAL VERSION ##
#publishURL <- "https://hiteshsm.cloudapp.net/workspaces/%s/webservices/%s" ## REMOVE SSL IGNORING FOR REAL VERSION ##
wrapper <- "inputDF <- maml.mapInputPort(1)\r\noutputDF <- matrix(ncol = %s, nrow = nrow(inputDF))\r\ncolnames(outputDF) <- list(%s)\r\noutputDF <- data.frame(outputDF)\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(paste(\"src\", file, sep=\"/\"), lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(strsplit(file, \"\\\\.\")[[1]][[1]], character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\nfor (i in 1:nrow(inputDF)) {\r\n  outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n}\r\nmaml.mapOutputPort(\"outputDF\")"



#############################################################
#' @title Get Function Source Code as a String
#' @description
#' This is a helper function that will convert a function's source code to a string
#' Also consider paste(body(fun())) or getAnywhere()
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
# TODO: add error handling at each step
# TODO: suppress the red text?
#############################################################
#' @title HELPER FUNCTION: Package Dependencies
#' @description
#' This is a helper function to extract object and package dependencies
#' then pack them into a .zip, then a base64 string
#' packDependencies()
#' @param closure functionName - function to package dependencies from
#' @return encoded zip - will return false if nothing was zipped
#############################################################
packDependencies <- function(functionName) {
  # lists for storing objects and packages
  dependencies = list()
  packages = list()

  # generate a GUID to act as a file name to store packages, R data
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))

  # NOTE: will not work if the user function specifies the names directly, e.g. won't find rjson::toJSON
  # from findGlobals man page: "R semantics only allow variables that might be local to be identified"
  # CONSIDER: how robust is this filtering? need to verify
  for (obj in codetools::findGlobals(get(functionName))) {
    name = get(obj)

    # filter out primitives and duplicates
    if (is.primitive(name) || (obj %in% names(dependencies))) {
      next
    }

    # get in-memory objects
    # Can nonfunction objects have dependencies???
    else if (!is.function(name)) {
      dependencies[[obj]] <- name
    }

    # grab user defined functions
    else if (identical(environment(name), globalenv())) {
      dependencies[[obj]] <- name
      # recursively get dependencies
      results <- recurDep(obj, dependencies, packages)
      dependencies <- results$dependencies
      packages <- results$packages
    }

    # get the names of packages of package functions
    # filter out base functions
    else if (paste(getNamespaceName(environment(name))) != "base") {
      # recursively get packages
      packages <- recurPkg(paste(getNamespaceName(environment(name))), packages)
    }

    # need an else branch?
  }

  # save current path to restore to later
  start = getwd()
  # go to package library, doing this to prevent zipping entire package
  toPack <- packages
  toZip = vector()
  for (i in 1:length(.libPaths())) {
    setwd(.libPaths()[i])
    # try to find and zip up the packages
    for (pkg in toPack) {
      if (file.exists(pkg)) {
        zip(paste(start, paste(pkg, "zip", sep="."), sep="/"), pkg)
        toZip <- c(toZip, paste(pkg, "zip", sep="."))
        toPack <- toPack[toPack != pkg]
      }
    }

    # if done packing, break
    if (length(toPack) == 0) {
      break
    }
    if (i == length(.libPaths())) {
      # error: can't find packages
      stop("Error: unable to locate packages. Please make sure the packages used are in at least one of the library paths.")
    }
  }
  # go back to where the user started
  setwd(start)

  # objects, functions, etc.
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
      # did I miss anything? maybe extra files floating around
      file.remove(paste(pkg, "zip", sep="."))
    }
    # delete the dependency rdta file
    file.remove(guid)
    file.remove(paste(guid,"zip",sep="."))

    # return the encoded zip as a string
    return(list(guid, zipEnc))
  }
  # if nothing was zipped, return false
  else {
    return(list(guid, ""))
  }
}



#############################################################
#' @title HELPER FUNCTION: Recursive Dependencies
#' @description This is helper function to recursively gather dependencies from user defined-functions
#' Similar structure to packDependencies()
#' recurDep()
#' @param string functionName - Name of function to recursively gather dependencies from
#' @param list dependencies - List of package dependencies
#' @param list packages - Name of available packages
#' @return list of packages and dependencies
#############################################################
recurDep <- function(functionName, dependencies, packages) {
  for (obj in codetools::findGlobals(get(functionName))) {
    name = get(obj)
    if (is.primitive(name) || (obj %in% names(dependencies))) {
      next
    }
    else if (!is.function(name)) {
      dependencies[[obj]] <- name
    }
    else if (identical(environment(name), globalenv())) {
      dependencies[[obj]] <- name
      results <- recurDep(obj, dependencies, packages)
      dependencies <- results$dependencies
      packages <- results$packages
    }
    else if (paste(getNamespaceName(environment(name))) != "base") {
      packages <- recurPkg(paste(getNamespaceName(environment(name))), packages)
    }
  }
  return(list("dependencies"=dependencies, "packages"=packages))
}



#############################################################
#' @title HELPER FUNCTION: Recursive Packaging
#' @description This is helper function to recursively gather dependencies from user defined-functions
#' recurPkg()
#' @param string pkgName - Name of package to check for existence in list of packages
#' @param list packages - Name of available packages
#' @return list of packages
#############################################################
recurPkg <- function(pkgName, packages) {
  # if the package isn't already in the list
  if (!(pkgName %in% packages)) {
    # add it
    packages <- c(pkgName, packages)
    pkgDeps <- available.packages()

    # if the package is available on a repo
    if (pkgName %in% row.names(available.packages())) {
      # iterate through the dependencies and check if need to add them
      for (pkg in strsplit(available.packages()[pkgName, "Depends"], split=", ")[[1]]) {
        # filter out duplicates and R version dependencies
        if (!(pkg %in% packages) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
          # recursively call recurPkg
          packages <- recurPkg(pkg, packages)
        }
      }
      # iterate through imports
      for (pkg in strsplit(available.packages()[pkgName, "Imports"], split=", ")[[1]]) {
        # filter out duplicates and R version dependencies
        if (!(pkg %in% packages) && !(grepl("R \\((.*)\\)", pkg)) && (pkg %in% row.names(available.packages()))) {
          # recursively call recurPkg
          packages <- recurPkg(pkg, packages)
        }
      }
    }
  }
  # return updated list of packages
  return(packages)
}




#############################################################
#' @title HELPER FUNCTION: Convert Format
#' @description This is a helper function to convert expected schema to API-expecting format
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
#'
#############################################################
publishWebService <- function(functionName, serviceName, inputSchema, outputSchema, wkID, authToken) {

  # Make sure input schema matches function signature
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

  # Generate unique guid
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))

  # API call
  RCurl::httpPUT(url = sprintf(publishURL, wkID, guid), # defined above
                 httpheader=c('Authorization' = paste('Bearer', authToken, sep=' '),
                              'Content-Type' = 'application/json',
                              'Accept' = 'application/json'),
                 content = body,
                 writefunction = h$update)

  # TODO: format output
  newService <- RJSONIO::fromJSON(h$value())

  # Use discovery functions to get default endpoint for immediate use
  # switch to getEndpoints() later
  defaultEP <- getEndpoints(wkID, authToken, newService["Id"], internalURL)

  # Curry relevant parameters to consumption function
  #consumption <- functional::Curry(consumeLists, "api_key"=defaultEP[[1]]["PrimaryKey"], "requestURL"=paste(defaultEP[[1]]["ApiLocation"],"/execute?api-version=2.0&details=true",sep=""), "columnNames"=as.list(names(inputSchema)))

  # currently returning list of webservice details, default endpoint details, consumption function, in that order
  return(list(newService, defaultEP))#, consumption))
}

