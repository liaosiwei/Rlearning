# svm to train a global model
library(e1071)

GetSvmInput <- function(dnormal) {
  ids <- as.integer(levels(factor(dnormal$user_id)))
  allmodeldata <- NULL
  for (id in ids) { # for normal dataset we build a global logistic model
    one <- dnormal[dnormal$user_id == id, ]
    allmodeldata <- rbind(allmodeldata, getmodeldata(one, 1))
  }
  allmodeldata$y <- as.factor(allmodeldata$y)
  allmodeldata
}
TrainSvmModel <- function(modeldata) {
  tuned <- svm(y~., data = modeldata)
}