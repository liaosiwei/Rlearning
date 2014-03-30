preprocess <- function(one_person_data) {
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

# x is one person data
click_buy <- function(x) {
  d <- with(x, xtabs(~ brand_id + type, x))
  if ("1" %in% dimnames(d)$type) {
    clicks_not_buy <- d[d[, 2] == 0, ][, c(1, 2)]
    clicks_buy <- d[d[, 2] == 1, ][, c(1, 2)]
    plot(clicks_not_buy, pch = 19, col = "red", main="clicks of buying vs. clicks of not buying",
         xlab="times of click", ylab="buy or not buy")
    points(clicks_buy, pch = 15, col = "blue")
  } else {
    clicks_not_buy <- d[, 1]
    plot(clicks_not_buy, matrix(1, nrow = length(clicks_not_buy), ncol = 1), pch = 19, col = "red")
  }
  legend("topright", inset=.05, title="buy and not buy", c("not buy","buy"), pch=c(19, 15), col=c("red", "blue"))
}