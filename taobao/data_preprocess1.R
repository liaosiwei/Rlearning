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

rules <- do_aprior(addtype)
# for all brand actions
