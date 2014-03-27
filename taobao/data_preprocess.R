data <- read.csv("E:/R/alibaba/t_alibaba_data.csv", head=TRUE)

data$visit_datetime = as.Date(strptime(data$visit_datetime, "%m月%d日"))

data <- data[order(data$user_id, data$brand_id, data$visit_datetime), ]

tdata <- aggregate(data, by=list(data$user_id, data$brand_id), cbind)

newdata <- tdata[, c(1, 2, 5)]
newdata <- newdata[order(newdata$Group.1, newdata$Group.2), ]

resdata <- c(0, 0, 0, 0, 0)

f <- function(x) {
  x[, 1]
}
uniqlist <- lapply(newdata[, 3], unique)

t <- sapply(uniqlist, f)

res <- as.data.frame(matrix(0, nrow=length(uniqlist), ncol=4))
colnames(res) <- c(paste("type", 1:4, sep=""))

for (i in 1:length(uniqlist)) {
  v = uniqlist[[i]][, 1] + 1
  res[i, v] = 1
}

input <- as(res, "transactions")

library("arules")
res_rule = apriori(input, parameter=list(supp=0.8, conf=0.9))
summary(res_rule)
inspect(res_rule)