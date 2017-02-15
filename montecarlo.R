# Distribucion uniforme en cuadrado

n <- 10000
coord_x <- runif(n)
coord_y <- runif(n)
plot(coord_x, 
     coord_y, 
     xlim =c(0,1), 
     ylim=c(0,1)
)

# Puntos dentro del cuarto de circulo
inside <- (coord_x^2 + coord_y^2 <= 1)

# Colorear de rojo los que están dentro
# points agrega puntos a la ultima grafica
points(
  coord_x[inside], # solo las coordenadas de x dentro
  coord_y[inside], # solo las coordenadas de y dentro
  bg = "red",  # color de fondo del marcador
  pch = 21 # tipo de marcador
)
# agregamos el semicirculo
linspace <- seq(0,1,.01)
lines(linspace, sqrt(1-linspace^2), col ="blue")

# fraccion dentro
mean(inside)
pi/4

# estimacion de pi por monte carlo
mean(inside)

# intervalos de confianza asintoticos
# por el teorema central del limite
confianza <- 0.95
alpha <- 1 - confianza
z <- qnorm(alpha/2, lower.tail = FALSE)
mu <- mean(inside)
sigma <- sd(inside)/sqrt(n)
lim_inf <- mu - sigma*z
lim_sup <- mu + sigma*z
sprintf("nsim:  %d, conf: %0.2f,  Int. de conf. (%0.6f, %0.6f)", 
        n,
        confianza,
        lim_inf, 
        lim_sup)

# bootstrapping
# n <- 100
# coord_x <- runif(n)
# coord_y <- runif(n)
n_replicas <- 100000
estimaciones <- numeric(n_replicas)
for (i in 1:n_replicas) {
  # remuestro
  id <- sample(1:n, replace = TRUE)
  boot_coord_x <- coord_x[id]
  boot_coord_y <- coord_y[id]
  # calcular estimador por cada remuestreo
  inside <- (boot_coord_x^2 + boot_coord_y^2 <= 1)
  estimaciones[i] <- mean(inside)
}
hist(estimaciones, xlim=c(0,1),
     main = "distribucion bootstrap de estimadores")
mean(estimaciones)
quantile(estimaciones, c(0.025, 0.975))
