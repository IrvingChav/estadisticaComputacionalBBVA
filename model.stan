data {
  // numero de obs
  int N;
  // variables del modelo
  // 1. independientes
  // tengo que decir es tipo continuo
  // y su longitud
  real edu[N]; // educacion
  real inc[N]; // income
  // 2. dependiente
  real prestige[N];
}

parameters {
  real beta_0;
  real beta_edu;
  real beta_inc;
  // algunos parametros requieren
  // restricciones, una desviacion
  // estandar no puede ser negativa
  // al incluir las restricciones 
  // evitamos que pueda haber algun
  // error
  real<lower=0> sigma;
}

transformed parameters {
  real prestige_hat[N];
  for (i in 1:N) {
    prestige_hat[i] = beta_0 + beta_inc*inc[i] + beta_edu*edu[i];
  }
}

model {
  // aprioris
  // pueden especificar aprioris que 
  // les interesen, si no especifican una
  // a priori, asume la apriori no 
  // informativa (constante...)
  beta_inc ~ normal(0.7, 1);
  beta_edu ~ normal(0.3, 0.5);
  // verosimilitud
  prestige ~ normal(prestige_hat, sigma);
  
}



