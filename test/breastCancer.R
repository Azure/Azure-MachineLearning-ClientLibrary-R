## k means clustering for breast cancer dataset##

setwd("C://Users/t-alewa/Documents/Azure-MachineLearning-ClientLibrary-R/test")
wsID = "3612640f27234eb7b2b91ac62e8b4a40"
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be"
dataset <- read.csv(file="breastCancer.csv")

# Create clustering function
getCluster <- function (age, mp, tSize, invNodes, nodeCaps, DegMalig, Breast, BreastQuad, Irradiat) {
  # Train model
  fit <- kmeans(rbind(dataset, data.frame("age"=age,"menopause"=mp, "tumor.size"=tSize, "inv.nodes"=invNodes, 
                                          "node.caps"=nodeCaps, "deg.malig"=DegMalig, "breast"=Breast, "breast.quad"=BreastQuad, 
                                          "irradiat"=Irradiat)),5)
  return(fit$cluster[[length(fit$cluster)]])
}

# Publish web service
onlineCluster <- publishWebService("getCluster", "kMeansCancer", list("age"="int","mp"="int", "tSize"="int", "invNodes"="int", 
                                                                      "nodeCaps"="int", "DegMalig"="int", "Breast"="int", "BreastQuad"="int", 
                                                                      "Irradiat"="int"), list("cluster"="int"), wsID, wsAuth)
# Consume web service
endpoints <- onlineCluster[[2]]
responseDF <- consumeDataTable(endpoints[[1]]$PrimaryKey, paste(endpoints[[1]]$ApiLocation,"/execute?api-version=2.0&details=true",sep=""), 
                               list("age", "mp", "tSize", "invNodes", "nodeCaps", "DegMalig", "Breast", "BreastQuad", "Irradiat"),
                               list(7, 2, 3, 4, 2, 3, 2, 1, 1))
