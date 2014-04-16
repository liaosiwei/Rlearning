# separate records from data after buying
filterdata <- function(one_person_data) {
  result <- one_person_data
  rest_mat <- matrix(NA, nrow = 0, ncol = 4, dimnames = list(NULL, c("user_id", "brand_id", "type", "visit_datetime")))
  brand_ids <- as.integer(levels(factor(one_person_data$brand_id)))
  for (id in brand_ids) {
    buy_mat <- result[result$brand_id == id & result$type == 1, ]
    if (nrow(buy_mat) > 0) {
      max_time <- max(buy_mat$visit_datetime)
      after_mat <- which(result$brand_id == id & result$visit_datetime > max_time)
      if (length(after_mat) > 0) {
        rest_mat <- rbind(rest_mat, result[after_mat, ])
        result <- result[-after_mat, ]
      }
    }
  }
  list(res = result, rest = rest_mat)
}

getone <- function(id) {
  return (data[data$user_id == id, ])
}

getPredictedData <- function(one) {
  # one: one user's data
  # return the data to input to getwpredicteddata
  result <- NULL
  brand_ids <- as.integer(levels(factor(one$brand_id)))
  for (id in brand_ids) {
    userdata <- one[one$brand_id == id, ]
    buy <- userdata[userdata$type == 1, ]
    if (nrow(buy) > 0) {
      max_time <- max(buy$visit_datetime)
      after_time <- which(userdata$visit_datetime > max_time)
      before_time <- which(userdata$visit_datetime <= max_time & userdata$type != 0 & userdata$type != 1)
      userdata <- userdata[c(after_time, before_time), ]
    }
    result <- rbind(result, userdata)
  }
  result
}

# get the user that his records only include buy
allsameuser <- function(data, type) {
  # data: original data
  # return: all users' id that their revords only include buy action
  allsame <- function(id) {
    one <- data[data$user_id == id, ]
    return (all(one$type == type))
  }
  user_ids = as.integer(levels(factor(data$user_id)))
  user_ids[unlist(lapply(user_ids, allsame))]
}

# split data as three parts: normal action, action only includes buy and that only includes clicks
splitdata <- function(data) {
  allbuy <- allsameuser(data, 1)
  allclick <- allsameuser(data, 0)
  buymat <- NULL
  clickmat <- NULL
  for (id in allbuy) {
    buymat <- rbind(buymat, data[data$user_id == id, ])
    data <- data[data$user_id != id, ]
  }
  for (id in allclick) {
    clickmat <- rbind(clickmat, data[data$user_id == id, ])
    data <- data[data$user_id != id, ]
  }
  return (list(normal = data, allbuy = buymat, allclick = clickmat))
}

seperatedata <- function(data) {
  user_ids <- as.integer(levels(factor(data$user_id)))
  allnoclickmat <- NULL
  clicknobuy <- NULL
  buynoclick <- NULL
  nobuynoclick <- NULL
  for (id in user_ids) {
    one <- data[data$user_id == id, ]
    if (0 %in% one$type & (!(1 %in% one$type))) {
      clicknobuy <- rbind(clicknobuy, one)
      data <- data[data$user_id != id, ]
    }
    if (!(0 %in% one$type) & 1 %in% one$type) {
      buynoclick <- rbind(buynoclick, one)
      data <- data[data$user_id != id, ]
    }
    if (!any(c(0, 1) %in% one$type)) {
      nobuynoclick <- rbind(nobuynoclick, one)
      data <- data[data$user_id != id, ]
    }
  }
  return (list(normal = data, cnb = clicknobuy, bnc = buynoclick, nbnc = nobuynoclick))
}