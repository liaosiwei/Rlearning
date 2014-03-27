data <- read.csv("E:/R/alibaba/t_alibaba_data.csv", head=TRUE)

data$visit_datetime = as.Date(strptime(data$visit_datetime, "%m月%d日"))

data <- data[order(data$user_id, data$brand_id, data$visit_datetime), ]

with(data, {tdata <<- aggregate(type ~ user_id + brand_id, data = data, FUN=cbind)})

len = nrow(tdata)

count <- function(len, tdata) {
  addtype <- matrix(0, nrow=len, ncol=4)
  for (i in 1:len) {
    x = tdata$type[[i]][, 1]
    typefactor = as.factor(x)
    count = tapply(x, typefactor, length)
    levelvec = as.integer(levels(typefactor)) + 1
    addtype[i, levelvec] = count
  }
  colnames(addtype) <- c(paste("type", 0:3, sep=""))
  addtype
}

addtype = count(len, tdata)

tdata <- cbind(tdata[,c(-3)], addtype)

library("arules")
indata <- as.data.frame(addtype)

indata[, 1] <- ordered(cut(indata[, 1], c(-1, mean(indata[, 1][indata[, 1] > 0]), Inf)), labels=c("low", "high"))
indata[, 2] <- ordered(cut(indata[, 2], c(-1, mean(indata[, 2][indata[, 2] > 0]), Inf)), labels=c("low", "high"))
indata[, 3] <- ordered(cut(indata[, 3], c(-1, mean(indata[, 3][indata[, 3] > 0]), Inf)), labels=c("low", "high"))
indata[, 4] <- ordered(cut(indata[, 4], c(-1, mean(indata[, 4][indata[, 4] > 0]), Inf)), labels=c("low", "high"))

rules <- apriori(indata, parameter=list(supp=0.8, conf=0.8))
rulesBuyLow <- subset(rules, subset = rhs %in% "type1=low")
rulesBuyHigh <- subset(rules, subset = rhs %in% "type1=high")
inspect(rulesBuyHigh)
inspect(rulesBuyLow)
