# Distribucion uniforme en cuadrado
n <- 100
n_replicates <- 10000
dist_estim <- replicate(n_replicates, {
  coord_x <- runif(n)
  coord_y <- runif(n)
  inside <- (coord_x^2 + coord_y^2 <= 1)
  mean(inside)
})
hist(dist_estim, breaks = 50, xlim=c(0,1))
quantile(dist_estim, c(0.025, 0.975))
