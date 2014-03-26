data <- read.csv("E:/R/alibaba/t_alibaba_data.csv", head=TRUE)

data$visit_datetime = as.Date(strptime(data$visit_datetime, "%m月%d日"))

data <- data[order(data$user_id, data$brand_id, data$visit_datetime), ]

tdata <- aggregate(data, by=list(data$user_id, data$brand_id), cbind)

newdata <- tdata[, c(1, 2, 5)]
newdata <- newData[order(newData$Group.1, newData$Group.2), ]

resdata <- c(0, 0, 0, 0, 0)
f <- function(x) {
  x[, 1]
}
uniqlist = lapply(newdata[, 3], unique)

t <- sapply(uniqlist, f)

res = matrix(nrow=length(uniqlist), ncol=5, dimnames=list(c(1:length(uniqlist)), c("id", "type1", "type2", "type3", "type4")))

