# filter one user's original data
datafilte <- function(data) {
  # param data: the original data ordered by user_id, brand_id and visit_datetime
  # return data frame like original data and 
  newdata <- sepdata(data)$res
  valid <- summary(factor(newdata$type), maxsum = Inf)
  
  if (!all(c("0", "1") %in% names(valid))) # tell that the user's action must include click and buy
    return (NULL)
  tab <- with(newdata, xtabs(~ brand_id + type, newdata))
  buy <- tab[tab[, 2] != 0, ]
  notbuy <- tab[tab[, 2] == 0, ]
  
  if (length(buy) == 0 | length(notbuy) == 0) # wired for xtab subseting
    return (NULL)
  
  if (is.null(nrow(buy)))
    buy = matrix(buy, nrow = 1)
  
  if (is.null(nrow(notbuy))) {
    notbuy = matrix(notbuy, nrow = 1)
  } else {
    notbuy <- notbuy[order(notbuy[, 1]), ]
    notbuy <- notbuy[c(1:round(0.9*nrow(notbuy))), ] # remove top clicks records
  }
  buy[, 1] <- buy[, 1] / buy[, 2] # average the click times as the buy times
  buy[, 2] <- 1
  d <- data.frame(x = c(notbuy[, 1], buy[, 1]), y = c(notbuy[, 2], buy[, 2])) # new data to pass to glm
  return (d)
}

# rewrite datafilte
getmodeldata <- function(data) {
  # param data: the original data ordered by user_id, brand_id and visit_datetime
  # return data frame like original data and 
  newdata <- sepdata(data)$res
  valid <- summary(factor(newdata$type), maxsum = Inf)
  
  if (!all(c("0", "1") %in% names(valid))) # tell that the user's action must include click and buy
    return (NULL)
  
  cols <- length(names(valid))
  tab <- with(newdata, xtabs(~ brand_id + type, newdata))
  buy <- tab[tab[, 2] != 0, ]
  notbuy <- tab[tab[, 2] == 0, ]
  
  buy <- matrix(buy, ncol = cols)
  notbuy <- matrix(notbuy, ncol = cols)
  
  if (nrow(buy) == 0 | nrow(notbuy) == 0) {
    print("y is not binary, actually is unary, e.g. user_id: 4054000")
    return (NULL)
  }
  if (nrow(notbuy) != 1) { 
    notbuy <- notbuy[order(notbuy[, 1]), ]
    notbuy <- notbuy[c(1:round(0.8*nrow(notbuy))), ] # remove top clicks records
  }
  buy[, 1] <- buy[, 1] / buy[, 2] # average the click times as the buy times
  buy[, 2] <- 1
  d <- data.frame(x = c(notbuy[, 1], buy[, 1]), y = c(notbuy[, 2], buy[, 2])) # new data to pass to glm
  return (d)
}

# get the data to be predicted
# TODO this predicted data may affect the result significantly
getpredicteddata <- function(data) {
  # param data: the original data ordered by user_id, brand_id and visit_datetime
  # return data frame which is the last month records of the user's
  startdate <- as.Date("2014-7-15")
  enddate <- as.Date("2014-8-15")
  last <- data[data$visit_datetime >= startdate & data$visit_datetime <= enddate & data$type == 0, ]
  if (nrow(last) == 0)
    return (NULL)
  res <- summary(factor(last$brand_id), maxsum = Inf)
  return (res)
}

# perform logistic regression on one user data
dopredict <- function(modeldata, predicteddata) {
  # param modeldata: the data used to pass to glm to train the model
  #       predicteddata: the data used to be predicted
  # return the predicted brand_id for the user
  glm_mod <- bayesglm(y ~ x, family = binomial(link="logit"), data = modeldata) # train the model
  out <- predict(glm_mod, data.frame(x = predicteddata), type = "response")
  return (names(out[out >= 0.5])) # return brand_ids which the probability is not less than 0.5
}


# perform logistic regression on all user data
predictall <- function(data) {
  # param data: all users' data ordered by user_id, brand_id and visit_datetime
  # return a list containing user's id and predicted brand ids
  wrapper <- function(id) {
    print(id)
    oneuser <- data[data$user_id == id, ]
    modeldata <- datafilte(oneuser)
    if (is.null(modeldata)) {
      return (NULL)
    }
    predicteddata <- getpredicteddata(oneuser)
    if (is.null(predicteddata)) {
      return (NULL)
    }
    return (dopredict(modeldata, predicteddata))
  }
  
  ids <- as.integer(levels(factor(data$user_id)))
  reslist <- lapply(ids, wrapper)
  names(reslist) <- ids
  return (reslist)
}

# save the brand ids for users into file using format: user_id\tbrand_id1 brand_id2... \n
savedata <- function(reslist, filename = "") {
  for (name in names(reslist)) {
    if (!is.null(reslist[[name]])) {
      cat(name, "\t", file = filename, sep = "", append = TRUE)
      cat(reslist[[name]], file = filename, append = TRUE)
      cat("\n", file = filename, sep = "", append = TRUE)
    }
  }
}