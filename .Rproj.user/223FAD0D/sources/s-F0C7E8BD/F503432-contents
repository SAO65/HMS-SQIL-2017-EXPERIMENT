mean <- 36
cv <- .25
sd <- mean * cv
lower.limit <- 1
upper.limit <- 72
n <- 1000

x <- rnorm(1000000000000, mean, sd); x <- x[x > lower.limit & x < upper.limit]
x
hist(x)


BaselineScore <- sample(rnorm(x, mean_baseline, sd_baseline), size = 7, replace = TRUE)
hist(BaselineScore)
