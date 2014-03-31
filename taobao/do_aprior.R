do_aprior <- function(input) {
  library("arules")
  indata <- as.data.frame(input)
  
  for (i in 1: 4) {
    indata[, i] <- ordered(cut(indata[, i], c(-1, 0, median(indata[, i][indata[, i] >  0]), Inf), labels=c("none", "low", "high")))
#     minvalue = min(indata[, i])
#     if (sum(indata[, i]) == 0) {
#       indata[, i] <- ordered(cut(indata[, i], c(-1, 0)), labels=c("none"))
#     }
#     else {
#       if (minvalue == 0) {
#         
#       }
#       else indata[, i] <- ordered(cut(indata[, i], c(0, median(indata[, i][indata[, i] >  0]), Inf), labels=c("low", "high")))
#     }
  }
  
  rules <- apriori(indata, parameter=list(supp=0.8, conf=0.8))
  buyrules <- subset(rules, subset = rhs %pin% "type1")
}