## k means clustering for breast cancer dataset##

wsID = "3612640f27234eb7b2b91ac62e8b4a40" #Replace with own workspace ID 
wsAuth = "abcbe14a958a40978f93aa0e0e71f5be" #Replace with own workspace authorization token 

dataset <- read.csv(file="breastCancer.csv")

# Create clustering function
getCluster <- function (age, mp, tSize, invNodes, nodeCaps, DegMalig, Breast, BreastQuad, Irradiat) {
  # Train model
  fit <- kmeans(rbind(dataset, data.frame("age"=age,"menopause"=mp, "tumor.size"=tSize, "inv.nodes"=invNodes,
                                          "node.caps"=nodeCaps, "deg.malig"=DegMalig, "breast"=Breast, "breast.quad"=BreastQuad,
                                          "irradiat"=Irradiat)),5)
  return(fit$cluster[[length(fit$cluster)]])
}
