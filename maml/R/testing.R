testing <- publishWebService("add", "e2eTest2", list("icol1"="int"), list("ocol1"="int"), wsID, auth)
endpoint <- getEndpointsT(wsID, auth, testing["Id"])
consumeSingleRequest(endpoint[[1]]["PrimaryKey"], paste(endpoint[[1]]["ApiLocation"],"/execute?api-version=2.0&details=true",sep=""), list("icol1"), list(1))

"inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(matrix(ncol = %s, nrow = nrow(inputDF)))\r\nfor (file in list.files(\"src\")) {\r\n  if (file == \"%s\") {\r\n    load(\"src/%s\")\r\n    for (item in names(dependencies)) {\r\n      assign(item, dependencies[[item]])\r\n    }\r\n  }\r\n  else {\r\n    if (!(file %%in%% installed.packages()[,\"Package\"])) {\r\n      install.packages(file, lib=\".\", repos=NULL, verbose=TRUE)\r\n    }\r\n    library(file, character.only=TRUE)\r\n  }\r\n}\r\naction <- %s\r\n  for (i in 1:nrow(inputDF)) {\r\n    outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n  }\r\nmaml.mapOutputPort(\"outputDF\")"

# No zip file / opening of dependencies
"inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(matrix(ncol = %s, nrow = nrow(inputDF)))\r\naction <- %s\r\n  for (i in 1:nrow(inputDF)) {\r\n    outputDF[i,] <- do.call(\"action\", as.list(inputDF[i,]))\r\n  }\r\nmaml.mapOutputPort(\"outputDF\")"

"outputDF <- data.frame(list.files(\"src\"))\r\nmaml.mapOutputPort(\"outputDF\")"
"inputDF <- maml.mapInputPort(1)\r\noutputDF <- data.frame(list.files(\"src\")\r\nmaml.mapOutputPort(\"outputDF\")"
