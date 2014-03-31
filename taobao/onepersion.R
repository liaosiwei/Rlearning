data <- read.csv("E:/R/alibaba/t_alibaba_data.csv", head=TRUE)

data$visit_datetime = as.Date(strptime(data$visit_datetime, "%m月%d日"))

data <- data[order(data$user_id, data$brand_id, data$visit_datetime), ]

with(data, {tdata <<- aggregate(type ~ user_id + brand_id, data = data, FUN=cbind)})

tdata <- tdata[order(tdata$user_id, tdata$brand_id), ]

len = nrow(tdata)

ids = as.integer(levels(as.factor(tdata$user_id)))

getrules <- function(id, indata) {
  input <- indata[indata$user_id == id,]
  do_aprior(count(nrow(input), input))
}

ruleslist = lapply(ids, getrules, tdata)
