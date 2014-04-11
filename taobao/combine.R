jdata <- read.csv("C:\\Users\\siwei\\Desktop\\result_mid.txt", sep = " ")
colnames(jdata) <- c("user_id", "brand_id", "count")

ids <- unique(jdata$user_id)

performpredict <- function(id) {
  modeldata <- getmodeldata(getone(id, data))
  predicteddata <- getone(id, jdata)$count
  names(predicteddata) <- getone(id,jdata)$brand_id
  model <- trainmodel(modeldata)
  if (is.null(model))
    return (NULL)
  return (dopredict(model, predicteddata))
}

getpredictedinput <- function(id) {
  predicteddata <- getone(id, jdata)$count
  names(predicteddata) <- getone(id,jdata)$brand_id
  return (predicteddata)
}

predictjiao <- function(data) {
  wrapper <- function(id) {
    print(id)
    oneuser <- data[data$user_id == id, ]
    modeldata <- getmodeldata(oneuser)
    if (is.null(modeldata)) {
      # cannot build model from modeldata
      # usually this kind of dataset cause this problem:
      # all brand that user have clicked was bought, so the model y is unary which is unnormal
      return (-1) 
    }
    predicteddata <- getpredictedinput(id)
    if (is.null(predicteddata)) {
      return (-2) # cannot get weighted predicted data
    }
    model <- trainmodel(modeldata)
    if (is.null(model)) {
      return (-3) # from the given dataset, we cann't build a logistic model
    }
    return (dopredict(model, predicteddata))
  }
  reslist <- lapply(ids, wrapper)
  names(reslist) <- ids
  return (reslist)
}

predictjiaoother <- function(normaldata, otherdata) {
  # data: the original ordered data, not unnormal data
  ids <- as.integer(levels(factor(normaldata$user_id)))
  allmodeldata <- NULL
  for (id in ids) { # for normal dataset we build a global logistic model
    one <- normaldata[normaldata$user_id == id, ]
    allmodeldata <- rbind(allmodeldata, getmodeldata(one, 1))
  }
  part <- allmodeldata[allmodeldata$y == 0, ]
  part <- part[order(part$x), ]
  part <- part[c(1:round(0.8 * nrow(part))), ] # cut off param: 0.8
  allmodeldata <- rbind(part, allmodeldata[allmodeldata$y == 1, ])
  globalmodel <- trainmodel(allmodeldata)
  
  wrapper <- function(id) {
    print(id)
    predicteddata <- getpredictedinput(id)
    if (is.null(predicteddata)) {
      return (NULL)
    }
    return (dopredict(globalmodel, predicteddata))
  }
  
  ids <- as.integer(levels(factor(otherdata$user_id)))
  reslist <- lapply(ids, wrapper)
  names(reslist) <- ids
  return (reslist)
}

mainV3 <- function(data, filename = "") {
  res <- seperatedata(data)
  
  # step 1: using normal data to predict
  reslist <- predictjiao(res$normal)
  # after step 1, we got 3 kinds of error because some users' data is not applicable for step 1
  # 1. normal brand id list
  # 2. -1 or -2, indicating error occurred, which is usually for getwpredicteddata or getmodeldata failed
  # 3. chr(0), indicating that we predicted nothing
  elist <- geterrlist(reslist)
  print(elist) # TODO handler this users' data
  savedata(reslist, filename)
  
  # step 2: using unnormal data to predict
  # the data include res$cnb, res$bnc, res$nbnc
#   other <- res$cnb
#   other <- rbind(other, res$bnc)
#   other <- rbind(other, res$nbnc)
#   for (id in elist$err1) {
#     other <- rbind(other, data[data$user_id == id, ])   # add dataset that y is no binary
#   }
  other <- NULL
  for (id in elist$err4) {
    other <- rbind(other, data[data$user_id == id, ])   # add dataset that cann't train a model
  }
  reslist <- predictjiaoother(res$normal, other)
  elist <- geterrlist(reslist)
  print(elist) # TODO handler this users' data
  savedata(reslist, filename)
}