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

getone <- function(id, data) {
  return (data[data$user_id == id, ])
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