testing <- publishWebService("add", "testPkg5", list("icol1"="int"), list("ocol1"="int"), wsID, auth)
endpoint <- getEndpointsT(wsID, auth, testing["Id"])
consumeSingleRequest(endpoint[[1]]["PrimaryKey"], paste(endpoint[[1]]["ApiLocation"],"/execute?api-version=2.0&details=true",sep=""), list("icol1"), list(1))
