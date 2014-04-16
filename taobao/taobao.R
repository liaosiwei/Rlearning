datecut <- seq(as.Date("2014/4/15"), as.Date("2014/7/15"), length.out = 14)

getTypeSeq <- function(d) {
    # d: one user's data in a specified time period
    # return the table value of actions in this time period
    x <- xtabs(~ brand_id + type, d)
    mat <- matrix(0, nrow = nrow(x), ncol = 5)
    mat[, 1] <- as.integer(dimnames(x)$brand_id)
    colnames(mat) <- c("brand_id", paste("type", c(0:3), sep = ""))
    cindex <- 1
    for (type in dimnames(x)$type) {
        mat[, as.integer(type)+2] <- as.integer(x[, cindex])
        cindex <- cindex + 1
    }
    mat
}

oneUserFeature <- function(one) {
    #labels <- floor(seq(1, 90, length.out = 14))
    
    y <- data.frame(brand_id = unique(one$brand_id))
    y <- cbind(y, matrix(0, nrow = length(y$brand_id), ncol = 4 * 13))
    for (i in seq(2, 14)) {
        d <- one[one$visit_datetime < datecut[i], ]
        mat <- getTypeSeq(d)
        w <- c(1, 2, 3, 4) + (i - 2)*4 + 1
        for (id in mat[, 1]) {
            y[y[, 1] == id, w] <- mat[mat[, 1] == id, c(-1)]
        }
    }
    y
}

oneTrainData <- function(one) {
    fmat <- oneUserFeature(one)
    fmat$label <- 0
    lastmonth <- one[one$visit_date > datecut[14] & one$type == 1, 2]
    if (length(lastmonth) > 0) {
        for (id in lastmonth) {
            fmat
        }
    }
}

oneUserLabel <- function(one) {
    
}

extractFeature <- function(data) {
    # data: original data
  
}