# strings for navigating
publishURL <- "https://hiteshsm.cloudapp.net/workspaces/%s/webservices/%s" ## REMOVE SSL IGNORING FOR REAL VERSION ##
wrapper <- "inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(matrix(ncol = %s, nrow = nrow(inputDF)))\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(paste(\"src\", file, sep=\"/\"), lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(strsplit(file, \"\\\\.\")[[1]][[1]], character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\n  for (i in 1:nrow(inputDF)) {\r\n    outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n  }\r\nmaml.mapOutputPort(\"outputDF\")"

##################################################################################
# WRAPPER TESTING
# Consider: assert statements (stopifnot), error handling
# wrap in a function that will return a string with the proper function
##################################################################################
# get the input
inputDF <- maml.mapInputPort(1)

# initialize an empty output dataframe of desired dimensions
outputDF <- data.frame(matrix(ncol = "%s", nrow = nrow(inputDF)))

for (file in list.files("src")) {
  if (file == "%s") {
    load("src/%s")
    # assert that dependencies exists?
    # NOTE: depedencies object comes from packDependencies(), maybe something more unique to avoid collisions?
    for (item in names(dependencies)) {
      assign(item, dependencies[[item]])
    }
  }
  else {
    # if the package isn't installed on Azure already, install it and its dependencies
    # need to recursively grab dependencies
    if (!(file %in% installed.packages()[,"Package"])) {
      install.packages(paste("src", file, sep="/"), lib= ".", repos=NULL, verbose=TRUE)
    }
    # load the package
    library(strsplit(file, "\\.")[[1]][[1]], character.only=TRUE)
  }
}

# user function
action <-

# apply function to every row
for (i in 1:nrow(inputDF)) {
  outputDF[i,] <- do.call("action", as.list(inputDF[i,]))
}

# return output
maml.mapOutputPort("outputDF")

# test function
add <- function(x) {
  print(findGlobals(add))
  return(x+a[[1]])
}



################################################################
# GET THE FUNCTION SOURCE CODE AS A STRING
# Return the function as a string
# Also consider paste(body(fun())) or getAnywhere()
################################################################
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
  return(gsub("\n", "\r\n", gsub("\"", "\\\"", objs)))
}



##################################################################################
# EXTRACT DEPENDENCIES
# Takes closure, not string
##################################################################################
packDependencies <- function(funName) {
  dependencies = list()
  packages = list()

  # generate a GUID to act as a file name to store packages, R data
  guid = gsub("-", "", uuid::UUIDgenerate(use.time=TRUE))

  # NOTE: will not work if the user function specifies the names directly, e.g. won't find rjson::toJSON
  # from findGlobals man page: "R semantics only allow variables that might be local to be identified"
  # CONSIDER: how robust is this filtering? need to verify
  for (obj in codetools::findGlobals(get(funName))) {
    name = get(obj)

    # filter out primitives
    if (is.primitive(name)) {
        next
    }

    # get objects
    else if (!is.function(name)) {
      dependencies[[obj]] <- name
    }

    # grab user defined functions
    else if (identical(environment(name), globalenv())) {
      dependencies[[obj]] <- name
    }

    # get the names of packages of package functions
    else if (paste(getNamespaceName(environment(name))) != "base") {
      packages[[obj]] <- getNamespaceName(environment(name))
    }

    # need an else branch?
  }

  # save current path to restore to later
  start = getwd()
  # go to package library, doing this to prevent tarballing entire path to package
  # TODO: what if packages are in different library directories? need to iterate through all paths
  setwd(.libPaths()[[1]])
  # list of things to include in aggregate .zip
  toZip = vector()
  # pack up each package in its own zip (.zip)
  for (pkg in packages) {
    # should error handle, e.g. if can't find package
    zip(paste(start, paste(pkg, "zip", sep="."), sep="/"), pkg)
    toZip <- c(toZip, paste(pkg, "zip", sep="."))
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



################################################################
# CONVERT FORMAT
# Helper function to convert expected schema to API-expecting format
################################################################
convert <- function(argList) {
  form <- list()
  for (arg in names(argList)) {
    type = argList[[arg]]
    # probably a better way to code this
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



################################################################
# expecting inputSchema = list("arg1"="type", "arg2"="type", ...)
# expecting outputSchema = list("output1"="type", "output2"="type", ...)
# funName is a string!!
################################################################
# TODO: play around with argument order
publishWebService <- function(funName, serviceName, inputSchema, outputSchema, wkID, authToken) {

  # Accept SSL certificates issued by public Certificate Authorities
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

  # Get and encode the dependencies
  zipString = packDependencies(funName)

  # Build the body of the request, differing on whether or not there is a zip to upload
  # Probably a more elegant way to do this
  if (zipString[[2]] == "") {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = convert(inputSchema),
        "OutputSchema" = convert(outputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), zipString[[1]], zipString[[1]], paste(getFunctionString(funName)))

      )
    )
  }
  else {
    req = list(
      "Name" = serviceName,
      "Type" = "Code",
      "CodeBundle" = list(
        "InputSchema" = format(inputSchema),
        "OutputSchema" = format(outputSchema),
        "Language" = "r-3.1-64",
        "SourceCode" = sprintf(wrapper, length(outputSchema), zipString[[1]], zipString[[1]], paste(getFunctionString(funName))),
#        "SourceCode" = "inputDF <- maml.mapInputPort(1)\r\ninstall.packages(paste(\"src\", \"codetools.zip\", sep=\"/\"), lib = \".\", repos=NULL)\r\nlibrary(\"codetools\")\r\noutputDF <- data.frame(findGlobals(findGlobals))\r\nmaml.mapOutputPort(\"outputDF\")",
#        "SourceCode" = "inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(list.files(\"src\"))\r\nmaml.mapOutputPort(\"outputDF\")",
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
                 writefunction = h$update,
                 ssl.verifyhost = FALSE) ### REMOVE THIS FOR THE REAL VERSION ###

  # return everything
  # TODO: format output
  rjson::fromJSON(h$value())
}
