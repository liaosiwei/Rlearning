library(DMwR)

getMD <- function(onedata) {
  newdata <- filterdata(onedata)$res
  valid <- summary(factor(newdata$type), maxsum = Inf)
  
  if (!all(c("0", "1") %in% names(valid))) # tell that the user's action must include click and buy
    return (NULL)
  
  cols <- length(names(valid))
  tab <- with(newdata, xtabs(~ brand_id + type, newdata))
  buy <- tab[tab[, 2] != 0, ]
  notbuy <- tab[tab[, 2] == 0, ]
  
  buy <- matrix(buy, ncol = cols)
  notbuy <- matrix(notbuy, ncol = cols)
  
  buy <- subset(buy, buy[, 1] != 0)
  notbuy <- subset(notbuy, notbuy[, 1] != 0)
  
  if (nrow(buy) == 0 | nrow(notbuy) == 0) {
    #print("y is not binary, actually is unary, e.g. user_id: 4054000")
    return (NULL)
  }
  buy[, 1] <- buy[, 1] / buy[, 2] # average the click times as the buy times
  buy[, 2] <- 1

  d <- data.frame(x = c(notbuy[, 1], buy[, 1]), y = c(notbuy[, 2], buy[, 2])) # new data to pass to glm
  d$y <- as.factor(d$y)
  k <- 0
  buyrow <- nrow(buy)
  if (buyrow < 6) {
    if (buyrow == 1) {
      d <- rbind(d, list(x = buy[1, 1], y = buy[1, 2]))
      k <- 1
    } else {
      k <- buyrow - 1
    }
  } else {
    k <- 5
  }
  SMOTE(y ~., d, perc.over=800, perc.under=113, k = k)
}

predictND <- function(data) {
  # param data: all users' data ordered by user_id, brand_id and visit_datetime
  # return a list containing user's id and predicted brand ids
  wrapper <- function(id) {
    #print(id)
    oneuser <- data[data$user_id == id, ]
    modeldata <- getMD(oneuser)
    if (is.null(modeldata)) {
      # cannot build model from modeldata
      # usually this kind of dataset cause this problem:
      # all brand that user have clicked was bought, so the model y is unary which is unnormal
      return (-1) 
    }
    predicteddata <- getwpredicteddata(oneuser, weight = c(1, 0, 4, 4))#, sigma = 0.5)
    if (is.null(predicteddata)) {
      return (-2) # cannot get weighted predicted data
    }
    model <- trainmodel(modeldata)
    if (is.null(model)) {
      return (-3) # from the given dataset, we cann't build a logistic model
    }
    return (dopredict(model, predicteddata))
  }
  
  ids <- as.integer(levels(factor(data$user_id)))
  reslist <- lapply(ids, wrapper)
  names(reslist) <- ids
  return (reslist)
}

trainGlobalModel <- function(nd) {
  ids <- as.integer(levels(factor(nd$user_id)))
  allmodeldata <- NULL
  for (id in ids) { # for normal dataset we build a global logistic model
    #print(id)
    one <- nd[nd$user_id == id, ]
    allmodeldata <- rbind(allmodeldata, getmodeldata(one, 1))
  }
  allmodeldata$y <- as.factor(allmodeldata$y)
  newdata <- SMOTE(y ~., allmodeldata, perc.over=600, perc.under=117)
  globalmodel <- trainmodel(newdata)
}

predictOD <- function(gModel, odata, weight = c(1, 0, 4, 4)) {
  # data: the original ordered data, unnormal data

  
  #print(summary(globalmodel)) #seems a very robust model
  
  wrapper <- function(id) {
    
    one <- odata[odata$user_id == id, ]
    predicteddata <- getwpredicteddata(one, weight = weight)
    if (is.null(predicteddata)) {
      return (NULL)
    }
    return (dopredict(gModel, predicteddata))
  }
  
  ids <- as.integer(levels(factor(odata$user_id)))
  reslist <- lapply(ids, wrapper)
  names(reslist) <- ids
  return (reslist)
}

mainV3 <- function(data, filename = "") {
  # step 1: using normal data to predict
  reslist <- predictND(res$normal)
  # after step 1, we got 3 kinds of error because some users' data is not applicable for step 1
  # 1. normal brand id list
  # 2. -1 or -2, indicating error occurred, which is usually for getwpredicteddata or getmodeldata failed
  # 3. chr(0), indicating that we predicted nothing
  elist <- geterrlist(reslist)
  #print(elist) # TODO handler this users' data
  printerror(elist)
  savedata(reslist, filename)
  print("------------------------------------------------")
  # step 2: using unnormal data to predict
  # the data include res$cnb, res$bnc, res$nbnc
  other <- res$cnb
  # other <- rbind(other, res$nbnc) # res$nbnc is empty
  for (id in elist$err1) {
    other <- rbind(other, data[data$user_id == id, ])   # add dataset that y is no binary
  }
  for (id in elist$err4) {
    other <- rbind(other, data[data$user_id == id, ])   # add dataset that cann't train a model
  }
  #   for (id in elist$err3) {
  #     other <- rbind(other, data[data$user_id == id, ])   # add dataset that cann't get output from their own model
  #   }
  gModel <- trainGlobalModel(res$normal)
  reslist <- predictOD(gModel, other)
  elist <- geterrlist(reslist)
  printerror(elist)
  #print(elist) # TODO handler this users' data
  savedata(reslist, filename)
  print("------------------------------------------------")
  # step 3: using global model to predict users which is res$bnc
  reslist <- predictOD(gModel, res$bnc, weight = c(0, 4, 4, 4))
  elist <- geterrlist(reslist)
  printerror(elist)
  #print(elist) # TODO handler this users' data
  savedata(reslist, filename)
}