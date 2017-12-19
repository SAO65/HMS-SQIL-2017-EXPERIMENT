# SIMULATION OF EXPECTED SCORE

a <- expand.grid(1:11,1:11, 1:11, 1:11, 1:11, 1:11)
b <- apply(a, 1, sum)
mean(b)
sd(b)

