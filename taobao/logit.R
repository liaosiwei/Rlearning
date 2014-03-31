logit <- function(one.user.data) {
  d <- with(one.user.data, xtabs(~ brand_id + type, one.user.data))
  if ("1" %in% dimnames(d)$type) {
    w <- (d[, 2] > 1)
    d[w, 1] <- d[w, 1] / d[w, 2]
    d[w, 2] <- 1
    y <- d[, 2]
    x <- d[, 1]
  } else {
    x <- d[, 1]
    y <- matrix(0, nrow = nrow(x), ncol = 1)
  }
  glm_mod <- glm(y ~ x, family = binomial(link="logit"), data = as.data.frame(cbind(y, x)))
}