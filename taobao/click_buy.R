# x is one person data
click_buy <- function(x) {
  d <- with(x, xtabs(~ brand_id + type, x))
  xmax = max(d[, 1])
  oldpar <- par(mfrow = c(3, 1))
  
  if ("1" %in% dimnames(d)$type) {
    clicks_not_buy <- d[d[, 2] == 0, ][, c(1, 2)]
    clicks_buy <- d[d[, 2] == 1, ][, c(1, 2)]
    plot(clicks_not_buy, pch = 19, col = "red", main="clicks of buying vs. clicks of not buying", xlim = c(0, xmax+1),
         xlab="times of click", ylab="buy or not buy")
    points(clicks_buy, pch = 15, col = "blue")
    legend("topright", inset=.05, title="buy and not buy", c("not buy","buy"), pch=c(19, 15), col=c("red", "blue"))
    
    smoothScatter(clicks_not_buy, xlim = c(0, xmax+1), main="times of click of not buying Colored by Smoothed Densities")
    smoothScatter(clicks_buy, xlim = c(0, xmax+1), main = "times of click of buying Colored by Smoothed Densities")
  } else {
    clicks_not_buy <- d[, 1]
    plot(clicks_not_buy, matrix(1, nrow = length(clicks_not_buy), ncol = 1), pch = 19, col = "red", xlim = c(0, xmax+1))
    legend("topright", inset=.05, title="buy and not buy", c("not buy","buy"), pch=c(19, 15), col=c("red", "blue"))
    smoothScatter(clicks_not_buy, xlim = c(0, xmax+1), main="times of click of not buying Colored by Smoothed Densities")
  }
  
  par(oldpar)
}

click_buy_wrapper <- function(one) {
  res <- preprocess(one)
  click_buy(res$res)
}