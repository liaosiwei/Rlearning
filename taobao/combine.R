jdata <- read.csv("result_mid.txt", sep = " ")
colnames(jdata) <- c("user_id", "brand_id", "count")

ids <- unique(jdata$user_id)

performpredict <- function(id) {
  modeldata <- getmodeldata(getone(id, data))
  predicteddata <- getone(id, jdata)$count
  names(predicteddata) <- getone(id,jdata)$brand_id
  model <- trainmodel(modeldata)
  if (is.null(model))
    return (NULL)
  return (dopredict(model, predicteddata))
}

reslist <- lapply(ids, performpredict)