# separate records from data after buying
sepdata <- function(one_person_data) {
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