# rewrite datafilte
getmodeldata <- function(data, c = 0.8) {
  # param data: the original one user's data ordered by user_id, brand_id and visit_datetime
  # c: a param that used to sampling data for modeling
  # return data frame like original data and 
  newdata <- filterdata(data)$res
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
  if (nrow(notbuy) / (nrow(buy) + nrow(notbuy)) > 0.6) { 
    notbuy <- notbuy[order(notbuy[, 1]), ]
    notbuy <- notbuy[c(1:round(c*nrow(notbuy))), , drop = FALSE] # remove top clicks records
  }
  buy[, 1] <- buy[, 1] / buy[, 2] # average the click times as the buy times
  buy[, 2] <- 1

  d <- data.frame(x = c(notbuy[, 1], buy[, 1]), y = c(notbuy[, 2], buy[, 2])) # new data to pass to glm
  return (d)
}



# get the data to be predicted
# TODO this predicted data may affect the result significantly
# getpredicteddata <- function(data) {
#   # param data: the original one user's data ordered by user_id, brand_id and visit_datetime
#   # return data frame which is the last month records of the user's
#   startdate <- as.Date("2014-7-15")
#   enddate <- as.Date("2014-8-15")
#   last <- data[data$visit_datetime >= startdate & data$visit_datetime <= enddate & data$type == 0, ]
#   if (nrow(last) == 0)
#     return (NULL)
#   res <- summary(factor(last$brand_id), maxsum = Inf)
#   return (res)
# }


gaussian <- function(input, sigma = 0.8) {
  # a gaussian function to get weigted month data
  return (exp(-input^2 / (2*sigma^2)))
}

getcount <- function(data, weight = c(1, 0, 3, 3)) {
  # data: one user's data
  # result is not ideal which p and r both decreased
  calwlength <- function(v) { # add weight to users' action. 1: 4: 4: 3
    len <- 0
    for (i in v) {
      len <- len + 1 * weight[i+1]
    }
    return (len)
  }
  return (aggregate(type ~ brand_id, data = data, FUN = calwlength))
}

getCount <- function(data, weight = c(1, 0, 3, 3)) {
    calwlength <- function(v) {
        len <- 0
        div <- 0
        wt <- weight
        wt[1] <- wt[1] / 2
        w <- which(v == 1)
        if (length(w) > 0) {
            div <- max(w)
        }
        k <- 1
        for (i in v) {
            if (k < div) {
                len <- len + 1 * wt[i+1]
            } else {
                len <- len + 1 * weight[i+1]
            }
            k <- k + 1
        }
        len
    }
    aggregate(type ~ brand_id, data = data, FUN = calwlength)
}

# get the data to be predicted
# add weight to data based on different month
getwpredicteddata <- function(data, weight = c(1, 0, 3, 3), sigma = 0.45) {
  # data: original one user's data
  #data <- getPredictedData(data)
  date <- as.Date(seq(as.Date("2014/4/15"), as.Date("2014/8/15"), length.out = 5))
  date[1] <- as.Date("2014/4/14")
  date[5] <- as.Date("2014/8/16") # cut doesn't include the edge of the range
  # data <- data[data$type == 0, ]
  if (nrow(data) == 0) {
    return (NULL) # for user's record that don't have click action. Maybe include buy or car or save action.. e.g.: user_id = 71250
  }
  datefact <- cut(as.Date(factor(data$visit_datetime)), date, labels = c(1, 2, 3, 4))
  
  res <- by(data, datefact, getCount, weight)
  mergedata <- data.frame(list(brand_id = unique(data$brand_id)))
  mergedata$num <- 0
  nms <- names(res)
  for (i in seq(nrow(mergedata))) {
    id <- mergedata[i, 1]
    tempval <- 0
    for (name in nms) {
      d <- res[[name]]
      if (!is.null(d)) {
        if (id %in% d$brand_id) {
          tempval <- tempval + d[d$brand_id == id, 2] * gaussian(4-as.numeric(name), sigma)
        }
      }
    }
    mergedata[i, 2] <- tempval
  }
  if (nrow(mergedata) == 0)
    return (NULL)
  value <- mergedata$num
  names(value) <- mergedata$brand_id
  value <- value[value > 1] 
  if (length(value) == 0)
    return (NULL)
  return (value)
}

trainmodel <- function(modeldata) {
  glm_mod <- glm(y ~ x, family = binomial(link="logit"), data = modeldata) # train the model
  s <- summary(glm_mod)
  if (nrow(s$coefficients) == 1)
    return (NULL)
  if (nrow(s$coefficients) == 2 & s$coefficients[2, 4] > 0.05)
    return (NULL)
  return (glm_mod)
}

# perform logistic regression on one user data
dopredict <- function(glm_mod, predicteddata) {
  # param glm_mod: the trained model
  #       predicteddata: the data used to be predicted
  # return the predicted brand_id for the user
  
  out <- predict(glm_mod, data.frame(x = predicteddata), type = "response")
  return (names(out[out >= 0.5])) # return brand_ids which the probability is not less than 0.5
}


# perform logistic regression on all user data
predictnormal <- function(data) {
  # param data: all users' data ordered by user_id, brand_id and visit_datetime
  # return a list containing user's id and predicted brand ids
  wrapper <- function(id) {
    #print(id)
    oneuser <- data[data$user_id == id, ]
    modeldata <- getmodeldata(oneuser)
    if (is.null(modeldata)) {
      # cannot build model from modeldata
      # usually this kind of dataset cause this problem:
      # all brand that user have clicked was bought, so the model y is unary which is unnormal
      return (-1) 
    }
    predicteddata <- getwpredicteddata(oneuser)
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

geterrlist <- function(reslist) {
  nobinary <- NULL
  nopredicted <- NULL
  nooutput <- NULL
  nomodel <- NULL
  for (name in names(reslist)) {
    v <- reslist[[name]]
    if (length(v) == 0) {
      nooutput <- cbind(nooutput, name)
    } else {
      if (length(v) == 1) {
        if (v[1] == -1) {
          nobinary <- cbind(nobinary, name)
        }
        if (v[1] == -2) {
          nopredicted <- cbind(nopredicted, name)
        }
        if (v[1] == -3) {
          nomodel <- cbind(nomodel, name)
        }
      }
    }
  }
  return (list(err1 = nobinary, err2 = nopredicted, err3 = nooutput, err4 = nomodel))
}

# get a global logistic regression model for users that only include one action.
predictother <- function(normaldata, otherdata) {
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
  
  # print(summary(globalmodel)) #seems a very robust model

  wrapper <- function(id) {
    # print(id)
    one <- otherdata[otherdata$user_id == id, ]
    predicteddata <- getwpredicteddata(one)
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

# save the brand ids for users into file using format: user_id\tbrand_id1 brand_id2... \n
savedata <- function(reslist, filename = "") {
  for (name in names(reslist)) {
    if (length(reslist[[name]]) > 0) {
      if (reslist[[name]] != -1 & reslist[[name]] != -2 & reslist[[name]] != -3) {
        cat(name, "\t", file = filename, sep = "", append = TRUE)
        cat(reslist[[name]], file = filename, sep = ",", append = TRUE)
        cat("\n", file = filename, sep = "", append = TRUE)
      }
    }
  }
}

main <- function(data, filename = "") {
  res <- splitdata(data)
  # normal means we can set a logistic regression model from one user's records, and get weighted predicted data
  # after predictnormal, the reslist contain three kinds of data:
  # 1. normal brand id list
  # 2. -1 or -2, indicating error occurred, which is usually for getwpredicteddata or getmodeldata failed
  # 3. chr(0), indicating that we predicted nothing
  reslist <- predictnormal(res$normal)
  # error list means the user id list that we cannot build model from the user's records
  error1list <- geterror1(reslist)
  savedata(reslist, filename)
  
  other <- res$allclick
  for (id in error1list) {
    other <- rbind(other, data[data$user_id == id, ])
  }
  reslist <- predictother(res$normal, other)
  savedata(reslist, filename)
}

printerror <- function(errlist) {
    cat("err1: not binary model datasets' length is", length(errlist[["err1"]]), "\n")
    cat("\t", errlist[["err1"]][c(1:10)], "\n")
    cat("err2: cannot get wpredicted datasets' length is", length(errlist[["err2"]]), "\n")
    cat("\t", errlist[["err2"]][c(1: 10)], "\n")
    cat("err3: no output length is", length(errlist[["err3"]]), "\n")
    cat("\t", errlist[["err3"]][c(1: 10)], "\n")
    cat("err4: cannot build model length is", length(errlist[["err4"]]), "\n")
    cat("\t", errlist[["err4"]][c(1: 10)], "\n")
}

mainV2 <- function(data, filename = "") {
  res <- seperatedata(data)
  
  # step 1: using normal data to predict
  reslist <- predictnormal(res$normal)
  # after step 1, we got 3 kinds of error because some users' data is not applicable for step 1
  # 1. normal brand id list
  # 2. -1 or -2, indicating error occurred, which is usually for getwpredicteddata or getmodeldata failed
  # 3. chr(0), indicating that we predicted nothing
  elist <- geterrlist(reslist)
  #print(elist) # TODO handler this users' data
  printerror(elist)
  print("------------------------------------------------")
  savedata(reslist, filename)
  
  # step 2: using unnormal data to predict
  # the data include res$cnb, res$bnc, res$nbnc
  other <- res$cnb
  other <- rbind(other, res$bnc)
  other <- rbind(other, res$nbnc)
  for (id in elist$err1) {
    other <- rbind(other, data[data$user_id == id, ])   # add dataset that y is no binary
  }
  for (id in elist$err4) {
    other <- rbind(other, data[data$user_id == id, ])   # add dataset that cann't train a model
  }
#   for (id in elist$err3) {
#     other <- rbind(other, data[data$user_id == id, ])   # add dataset that cann't get output from their own model
#   }
  reslist <- predictother(res$normal, other)
  elist <- geterrlist(reslist)
  printerror(elist)
  #print(elist) # TODO handler this users' data
  savedata(reslist, filename)
}

mainV4 <- function(filename = "") {
    # step 1: using normal data to predict
    gModel <- trainGlobalModel(res$normal)
    reslist <- predictOD(gModel, res$normal, sigma = 0.75)
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
    
    reslist <- predictOD(gModel, other)
    elist <- geterrlist(reslist)
    printerror(elist)
    #print(elist) # TODO handler this users' data
    savedata(reslist, filename)
    print("------------------------------------------------")
    # step 3: using global model to predict users which is res$bnc
    reslist <- predictOD(gModel, res$bnc, weight = c(0, 2, 4, 4), sigma = 0.8)
    elist <- geterrlist(reslist)
    printerror(elist)
    #print(elist) # TODO handler this users' data
    savedata(reslist, filename)
}