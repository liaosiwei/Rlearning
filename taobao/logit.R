logit <- function(one.user.data) {
  d <- with(one.user.data, xtabs(~ brand_id + type, one.user.data))

  if ("1" %in% dimnames(d)$type && "0" %in% dimnames(d)$type) {
    w <- (d[, 2] > 1)
    d[w, 1] <- d[w, 1] / d[w, 2]
    d[w, 2] <- 1
    y <- d[, 2]
    x <- d[, 1]
  } else {
    x <- d[, 1]
    y <- matrix(0, nrow = length(x), ncol = 1)
    #print ("action of user seems unnormal which don't have at least one buying action in all 4 month records")
    return (FALSE)
  }
  dd <- as.data.frame(cbind(y, x))
  glm_mod <- glm(y ~ x, family = binomial(link="logit"), data = dd)
  #plot(y ~ x, dd)
  glm_mod
}

CoeProb <- function(id) {
  print(id)
  one <- data[data$user_id == id, ]
  newdata <- sepdata(one)
  mylogit <- logit(newdata$res)
  if (is.logical(mylogit)) {
    #print ("didn't perform logit regression as input data format is not fit")
    return (FALSE)
  }
  s <- summary(mylogit)
  #print (s)
  # print(s$coefficients)
  if (nrow(s$coef) < 2) {
    return (FALSE)
  }
  pr <- s$coefficients[2, 4]
  if (pr > 0.05) {
    #print ("relation between click times and buying or not buying is not strong enough to perform regression")
    return (FALSE)
  }
  start.date <- as.Date("2014-7-15")
  end.date <- as.Date("2014-8-15")
  one.last.month.data <- one[one$visit_datetime >= start.date & one$visit_datetime <= end.date, ]
  predict.data <- sepdata(one.last.month.data)
  
  if (nrow(predict.data$rest) == 0) {
    t <- with(predict.data$res, xtabs(~ brand_id + type, predict.data$res))
  } else {
    t <- with(predict.data$rest, xtabs(~ brand_id + type, predict.data$rest))
  }
  if (ncol(t) == 0) {
    return (FALSE)
  }
  # print(t)
  input <- data.frame(x = t[, 1])
  input$result <- predict(mylogit, input, type="response")
  input
}

Filter <- function(x) {
  # x: result from CoeProb function
  if (is.logical(x)) {
    return (FALSE)
  }
  w <- x[, 2] > 0.5
  dimnames(x)[[1]][w]
}

compose <- function(id) {
  list(user_id = id, brand_ids = Filter(CoeProb(id)))
}

PredictWrapper <- function(input) {
  # input: the ordered data
  ids <- as.integer(levels(factor(input$user_id)))
  res <- lapply(ids, compose)
}

PrintOut <- function(reslist) {
  for (one in reslist) {
    if (!is.logical(one$brand_ids)) {
      if (length(one$brand_ids) != 0) {
        cat(one$user_id, "\t", one$brand_ids, "\n")
      }
    }
  }
}
