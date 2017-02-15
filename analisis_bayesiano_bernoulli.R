# muestra de experimentos Bernoulli(p)
# con p desconocido
x <- c(0,0,1,0,0,1,0,0,1,0,1,1,0,0,0,0)
table(x)
# ======= ANALISIS FRECUENTISTA ======
# maxima verosimilitud
mean(x)

# ======= ANALISIS BAYESIANO ===================

# ____paso 1. informacion previa ------------
apriori <- function(p, alpha, beta) {
  dbeta(p, alpha, beta)
}
# graficar la apriori
alpha = 4
beta = 2
domain <- seq(0, 1, .01)
dist_apriori <- apriori(domain, alpha, beta)
plot(domain, dist_apriori, type = "l", main = "dist apriori")

# ____paso 2. verosimilitud ---------------
likelihood <- function(p, x) {
  p^(sum(x)) * (1-p)^(length(x) - sum(x))
}
# graficamos verosimilitud
domain <- seq(0, 1, .01)
dist_likelihood <- likelihood(domain, x)
plot(domain, 
     dist_likelihood, 
     type = "l", 
     main = "verosimilitud"
)

# ____paso 3. a posteriori ------------
aposteriori <- function(p, x, alpha, beta) {
  dbeta(p, 
        alpha + sum(x), 
        beta + length(x) - sum(x))
}
domain <- seq(0, 1, .01)
dist_aposteriori <- aposteriori(domain, x, alpha, beta)
plot(domain, 
     dist_aposteriori, 
     type = "l", 
     main = "posterior"
)

# ____paso 4. Analisis de la posterior ------
#' conociendo la aposteri puedo decir
#' muchas cosas de el parametro desconocido
#' por ejemplo, la media, varianza, moda
#' o intervalos de confianza de la distribucion
#' para la mayoria de estos estadisticos
#' hay que resolver ecuaciones integrales
#' pero... puedo dar el estimador posterior
#' de la moda o MAP
MAP <- domain[which.max(dist_aposteriori)]
MAP

# ____paso 5. graficar todas las distrib juntas
plot(domain, 
     dist_apriori, 
     type = "l",
     col = "blue",
     ylim = c(0, 6))
# para poder graficar la verosimilitud 
# necesitamos convertirla en densidad
constante_latosa <- integrate(
  function(u) likelihood(u, x),
  lower = 0, upper = 1
)
lines(domain,
      dist_likelihood/constante_latosa$value,
      col = "red")
lines(domain,
      dist_aposteriori,
      col = "green")
legend("topright", 
       c("a priori", "verosimilitud", "a posteriori"),
       col = c("blue", "red", "green"),
       lwd = 1)

# con ggplot
library(ggplot2)
library(tidyr)
plot_data <- data.frame(
  p = domain,
  apriori = dist_apriori,
  verosimilitud = dist_likelihood/constante_latosa$value,
  aposteriori = dist_aposteriori
)
plot_data <- gather(
  plot_data,
  key = distribucion,
  value =  densidad,
  -p
)
p <- ggplot(plot_data, aes(x = p, 
              y = densidad, 
              color = distribucion)) +
  geom_line()

#
library(plotly)
ggplotly(p)
